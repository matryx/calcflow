;;; -*- Mode:LISP; Package:MACSYMA -*-
;;	** (c) Copyright 1981 Massachusetts Institute of Technology **
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
;; Comments:
;;
;; The Itensor package was downcased, cleaned up, and moving frames
;; functionality was added by Viktor Toth (http://www.vttoth.com/).
;;
;; As of November, 2004, the naming conventions in this package now
;; correspond with the naming conventions in commercial MACSYMA.
;;

(in-package :maxima)

(macsyma-module itensor) ;; added 9/24/82 at UCB

(cond (($get '$itensor '$version) (merror "ITENSOR already loaded"))
      (t ($put '$itensor '$v20081223 '$version)))

;    Various functions in Itensor have been parceled out to separate files. A
;    function in one of these files will only be loaded in (automatically) if
;    explicitly used in the Maxima. (It is necessary to have first loaded in
;    ITENSOR FASL for this autoloading to take place.) The current status of
;    these separate files are:

;    Filename          Macsyma Functions
;    --------          -----------------
;    CANTEN FASL       CANTEN, CONCAN, IRPMON
;    GENER FASL        IC_CONVERT, MAKEBOX, AVERAGE, CONMETDERIV, FLUSH1DERIV,
;                      IGEODESIC_COORDS
;    SYMTRY FASL       CANFORM, DECSYM, DISPSYM, REMSYM

(autof '$ic_convert '|gener|)
(autof '$decsym '|symtry|)
(autof '$canform '|symtry|)
(autof '$canten '|canten|)
(autof '$makebox '|gener|)
(autof '$igeodesic_coords '|gener|)
(autof '$conmetderiv '|gener|)
(autof '$name '|canten|)

(declare-top (special smlist $idummyx $vect_coords $imetric $icounter $dim
		      $contractions $coord $allsym $metricconvert $iframe_flag
		      $itorsion_flag $inonmet_flag))

(setq $idummyx '$%                   ;Prefix for dummy indices
      $icounter 0.                   ;Dummy variable numeric index
      smlist '(mlist simp)           ;Simplified mlist header
      $vect_coords nil               ;Used when differentiating w.r.t. a number
      $coord '((mlist simp))         ;Objects treated liked coordinates in diff
      $allsym nil                    ;If T then all indexed objects symmetric
      $metricconvert t               ;Flag used by $ic_convert
      $iframe_flag nil
      $itorsion_flag nil)

(defmacro ifnot  (&rest clause) `(or ,@ clause))

(defmacro m+or*or^p (&whole cl &rest ign)
  (declare (ignore ign))
  (subst (cadr cl)
	 'x
	 '(member (caar x) '(mtimes mplus mexpt) :test #'eq)))

(defmfun $idummy ()                              ;Sets arguments to dummy indices
  (progn
    (incf $icounter)
    (intern (format nil "~a~d" $idummyx $icounter))))

(defprop $kdelta ((/  . / )) contractions)

(defun isprod (x)
  (or (equal x '(mtimes)) (equal x '(mtimes simp))
      (equal x '(mtimes simp ratsimp))))

;; Remove occurrences of ratsimp from elements of x
(defun derat (x)
  (cond
    ((null x) nil)
    ((atom x) x)
    ((eq (car x) 'ratsimp) (derat (cdr x)))
    (t (cons (derat (car x)) (derat (cdr x))))
  )
)

(defun plusi(l)
  (cond
    ((null l) l)
    ((and (numberp (car l)) (< (car l) 0)) (plusi (cdr l)))
    ((atom (car l))  (cons (car l) (plusi (cdr l))))
    ((and (isprod (caar l)) (eq (cadar l) -1)) (plusi (cdr l)))
    (t (cons (car l) (plusi (cdr l))))
  )
)

(defun minusi(l)
  (cond
    ((null l) l)
    ((and (numberp (car l)) (< (car l) 0)) (cons (neg (car l)) (plusi (cdr l))))
    ((atom (car l))  (minusi (cdr l)))
    (
      (and (isprod (caar l)) (eq (cadar l) -1)) 
      (cons (caddar l) (minusi (cdr l)))
    )
    (t (minusi (cdr l)))
  )
)


(defun covi (rp) (plusi (cdadr rp)))
(defun conti (rp) (append (minusi (cdadr rp)) (cdaddr rp)))
(defun deri (rp) (cdddr rp))
(defun name (rp) (caar rp))
(defmfun $covi (rp) (cond ((rpobj rp) (cons smlist (covi rp)))
                          (t (merror "Not an RPOBJ"))
                    )
)
(defmfun $conti (rp) (cond ((rpobj rp) (cons smlist (conti rp)))
                                       (t (merror "Not an RPOBJ"))
                     )
)
(defmfun $deri (rp) (cond ((rpobj rp) (cons smlist (deri rp)))
                                      (t (merror "Not an RPOBJ"))
                    )
)
(defmfun $name (rp) (cond ((rpobj rp) (caar rp)) (t (merror "Not an RPOBJ"))))

;KDELTA has special contraction property because it contracts with any indexed
;object.

(meval '(($declare) %kdelta $constant))          ;So derivative will be zero
(meval '(($declare) $kdelta $constant))          ;So derivative will be zero
(meval '(($declare) %levi_civita $constant))
(meval '(($declare) $levi_civita $constant))

(setq $dim 4. $contractions '((mlist simp))) 

(defmfun $defcon n            ;Defines contractions: A contracts with B to form C
       ((lambda (a)
	 (add2lnc a $contractions)
	 (putprop
	  a
	  (cons (cond ((= n 1.) '(/  . / ))
		      ((= n 3.) (cons (arg 2.) (arg 3.)))
		      (t (merror "DEFCON takes 1 or 3 arguments")))
		(zl-get a 'contractions))
	  'contractions)
	 '$done)
	(arg 1.))) 

(defmspec $dispcon (a) (setq a (cdr a))
  ;;Displays contraction definitions
       ((lambda (tmp) 
	 (and (eq (car a) '$all) (setq a (cdr $contractions)))
	 (cons
	  smlist
	  (mapcar 
	   #'(lambda (e) 
	     (cond ((setq tmp (zl-get e 'contractions))
		    (cons smlist
			  (mapcar #'(lambda (z) 
					   (cond ((eq (car z)
						      '/ )
						  (list smlist e))
						 (t (list smlist
							  e
							  (car z)
							  (cdr z)))))
				  tmp)))
		   (t '((mlist simp)))))
	   a)))
	nil)) 

(defmspec $remcon (a) (setq a (cdr a))
  ;;Removes contraction definitions
       (and (eq (car a) '$all) (setq a (cdr $contractions)))
       (cons smlist (mapc #'(lambda (e)
			      (zl-remprop e 'contractions)
			      (setq $contractions (delete e $contractions :test #'eq)))
			  a)))

(defun getcon (e)
  ;; Helper to obtain contractions on both the noun and verb form of E
	(cond ((and (symbolp e) (eq (getcharn e 1) #\%))  (zl-get ($verbify e) 'contractions))
		(t (zl-get e 'contractions))
	)
)

(defun rpobj (e)                  ;"True" if an indexed object and not a matrix
       (cond ((and (not (atom e)) (eq (caar e) 'mqapply)) (rpobj (cdr e)))
	     (t 
       (and (not (atom e))
	    (not (eq (caar e) '$matrix))
	    ($listp (cadr e))
	    (cond ((cddr e) ($listp (caddr e)))
		  (t (nconc e '(((mlist simp))))  t  ))))))
                                          ;Transforms F([...]) into F([...],[])

;RPOBJ is the predicate for indexed objects. In the case of no contravariant
;components, it tacks a null list on.

(deff $tenpr #'rpobj)

(defmfun $imetric (v) (setq $imetric v) ($defcon v) ($defcon v v '$kdelta))

(defun mysubst0 (new old)                  ;To reuse subparts of old expression
       (cond ((alike1 new old) old) (t new))) 

(defun cov (a b)                            ;COV gives covariant form of metric
       (cond ((boundp '$imetric)
	      (meval (list (ncons $imetric)
			   (list smlist a b)
			   '((mlist simp)))))
	     (t (merror "Name of metric must be specified"))))

(defun contr (a b)                      ;contr gives contraviant form of metric
       (cond ((boundp '$imetric)
	      (meval (list (ncons $imetric)
			   '((mlist simp))
			   (list smlist a b))))
	     (t (merror "Name of metric must be specified"))))

(defun diffcov (a b d)
	(cond ((boundp '$imetric)
		(meval (list (ncons $imetric)
			   (list smlist a b)
			   '((mlist simp))
				d
			)

		))
		(t (merror "Name of metric must be specified"))))

(defmfun $ichr1 nargs                   ; Christoffel-symbol of the first kind
  (prog (a b c)
    (cond 
      (
        (> nargs 2) ; Derivative indices present; use idiff() to resolve
        (return
          (meval
            (cons
              '$idiff
              (cons
                ($ichr1 (arg 1) (arg 2))
                (apply
                  #'append
                  (mapcar #'(lambda (e) (list e 1)) (cddr (listify nargs)))
                )
              )
            )
          )
        )
      )
      (
        (> nargs 1)
        (and (eq 1 (length (arg 2))) (return ($ichr1 (arg 1))))
        (merror "ichr1 cannot have contravariant indices")
      )
      (t            ; G_abc = 1/2*(g_ba,c+g_ca,b-g_bc,a)
        (setq a (cadddr (arg 1)) b (cadr (arg 1)) c (caddr (arg 1)))
        (return
          (list
            '(mtimes)
            '((rat simp) 1. 2.)
            (list
              '(mplus)
              (diffcov b a c)
              (diffcov c a b)
              (list '(mtimes) -1. (diffcov b c a))
            )
          )
        )
      )
    )
  )
)

(defmfun $ichr2 nargs                   ; Christoffel-symbol of the second kind
  (prog (a b c d) 
    (cond
      (
        (> nargs 2) ; Derivative indices present; use idiff() to resolve
        (return
          (meval
            (cons
              '$idiff
              (cons
                ($ichr2 (arg 1) (arg 2))
                (apply
                  #'append
                  (mapcar #'(lambda (e) (list e 1)) (cddr (listify nargs)))
                )
              )
            )
          )
        )
      )
      (t            ; G_ab^c=g^cd*G_abd
        (setq a (cadr (arg 1)) b (caddr (arg 1)) c (cadr (arg 2)))
        (return
          (do
            ((flag) (l (append (cdr (arg 1)) (cdr (arg 2)))))
            (flag
              (list '(mtimes) (contr c d) ($ichr1 (list smlist a b d)))
            )
            (setq d ($idummy))
            (and (not (member d l :test #'eq)) (setq flag t))
          )
        )
      )
    )
  )
)

(defmfun $icurvature (l1 l2) 
  (prog (i j k h r) 
    (setq r ($idummy) i (cadr l1) k (caddr l1) h (cadddr l1) j (cadr l2))
    (return
      (list
        '(mplus)
        (idiff (list (diffop) (list smlist i k) l2) h)
        (list
          '(mtimes) -1.
          (idiff (list (diffop) (list smlist i h) (list smlist j)) k)
        )
        (list
          '(mtimes)
          (list (diffop) (list smlist i k) (list smlist r))
          (list (diffop) (list smlist r h) l2)
        )
        (list
          '(mtimes)
          -1.
          (list (diffop) (list smlist i h) (list smlist r))
          (list (diffop) (list smlist r k) l2)
        )
        (cond
          (
            $iframe_flag
            (list
              '(mtimes) -1.
              (list '($ifb) (list smlist k h) (list smlist r))
              (list '($icc2) (list smlist r i) (list smlist j))
            )
          )
          (t 0.)
        )
      )
    )
  )
) 

(defun covsubst (x y rp)       ;Substitutes X for Y in the covariant part of RP
       (cons (car rp) (cons (subst x y (cadr rp)) (cddr rp)))) 

(defun consubst (x y rp)   ;Substitutes X for Y in the contravariant part of RP
       (cons (car rp)
	     (cons (cadr rp)
		   (cons (subst x y (caddr rp)) (cdddr rp))))) 

(defun dersubst (x y rp)   ;Substitutes X for Y in the derivative indices of RP
       (nconc (list (car rp) (cadr rp) (caddr rp))
	      (subst x y (cdddr rp)))) 

;; COVARIANT DIFFERENTIATION
;; As of November, 2004, COVDIFF now takes into account the value of
;; iframe_flag. If true, COVDIFF uses the coefficients icc2 in place
;; of the Christoffel-symbols ichr2.

(defun diffop ()                ; ichr2 or icc2 depending on iframe_flag
  (cond
    (
      (or $iframe_flag $itorsion_flag $inonmet_flag)
      '($icc2 simp)
    ) 
    (t '($ichr2 simp))
  )
)

(declare-top (special x temp d)) 

(defmfun $covdiff nargs
  (prog
    (x e temp d i)
    (and (< nargs 2) (merror "COVDIFF must have at least 2 args"))
    (setq i 2 e (arg 1))
    again (setq x (arg i) e (covdiff e) i (1+ i))
    (and (> i nargs) (return e))
    (go again)
  )
)

(defun covdiff (e)                      ; The covariant derivative...
  (setq d ($idummy))
  (cond
    (               ; is the partial derivative for scalars (*** torsion?)
      (or (atom e) (eq (caar e) 'rat))
      (idiff e x)
    )
    (
      (rpobj e)
      (setq temp
        (mapcar 
          #'(lambda (v)
            (list '(mtimes)
              (list (diffop) (list smlist d x) (list smlist v))
              (consubst d v e)
            )
          )
          (cdaddr e)
        )
      )
      (simplus
        (cons
          '(mplus)
          (cons
            (idiff e x)
            (cond
              (
                (or (cdadr e) (cdddr e))
                (cons (list '(mtimes) -1.  (cons '(mplus)
                      (nconc
                        (mapcar 
                          #'(lambda (v) 
                            (list '(mtimes)
                                (list
                                  (diffop)
                                  (list smlist v x)
                                  (list smlist d)
                                )
                                (covsubst d v e)
                            )
                          )
                          (cdadr e)
                        )
                        (mapcar 
                          #'(lambda (v) 
                            (list
                              '(mtimes)
                              (list 
                                (diffop)
                                (list smlist v x)
                                (list smlist d)
                              )
                              (dersubst d v e)
                            )
                          )
                          (cdddr e)
                        )
                      )
                    )
                  )
                  temp
                )
              )
              (t temp)
            )
          )
        )
        1. t
      )
    )
    (
      (eq (caar e) 'mtimes)     ; (a*b)'
      (simplus
        (covdifftimes (cdr e) x)
        1 t
      )
    )
    (
      (eq (caar e) 'mplus)      ; (a+b)'=a'+b'
      (simplifya
        (cons
          '(mplus)
          (mapcar 'covdiff (cdr e))
        )
        nil
      )
    )
    (
      (eq (caar e) 'mexpt)      ; (a^b)'=b*a^(b-1)*a'
      (simptimes
        (list
          '(mtimes)
          (caddr e)
          (list
            '(mexpt)
            (cadr e)
            (list '(mplus) -1. (caddr e))
          )
          ($covdiff (cadr e) x)
        )
        1. nil
      )
    )
    (
      (eq (caar e) 'mequal)
      (list (car e) (covdiff (cadr e)) (covdiff (caddr e)))
    )
    ((eq (caar e) '%determinant) 0)
    (t (merror "Not acceptable to COVDIFF: ~M" (ishow e)))
  )
)

(defun covdifftimes (l x) 
  (prog (sp left out) 
    (setq out (ncons '(mplus)))
    loop (setq sp (car l) l (cdr l))
    (nconc out
      (list
        (simptimes
          (cons '(mtimes) (cons ($covdiff sp x) (append left l)))
          1. t
        )
      )
    )
    (cond ((null l) (return out)))
    (setq left (nconc left (ncons sp)))
    (go loop)
  )
) 

(declare-top (unspecial r temp d)) 

(defun vecdiff (v i j d) ;Add frame bracket contribution when iframe_flag:true
  (cond
    (
      $iframe_flag
      (cons
        '(mplus simp)
        (list
          (list (list v) '((mlist)) (list '(mlist) i) j)
          (list
            '(mtimes simp)
            (list (list v) '((mlist)) (list '(mlist) d))
            (list
              '(mtimes simp)
              -1.
              (list '(%ifb) (list '(mlist) d j) (list '(mlist) i))
            )
          )
        )
      )
    )
    (t
      (list (list v) '((mlist)) (list '(mlist) i) j)
    )
  )
)

(defun liediff (v e n)
  (cond
    ((not (symbolp v)) (merror "~M is not a symbol" v))
    (
      (or (atom e) (eq (caar e) 'rat)) ; Scalar field
                                       ; v([],[%1])*idiff(e,%1)
      (let
        ((dummy (implode (nconc (exploden $idummyx) (exploden n)))))
        (list
          '(mtimes) (list (list v) '((mlist)) (list '(mlist) dummy))
          ($idiff e dummy)
        )
      )
    )
    (
      (rpobj e)                        ; Tensor field

;     Dummy implementation for logic tests
;     (list '(%liediff) v e)

;     Shall the dummy index be in ICOUNTER sequence? Probably yes.
;     (let ((dummy (implode (nconc (exploden $idummyx) (exploden n)))))
      (let
        (
          (dummy ($idummy))
          (dummy2
            (cond
              ($iframe_flag ($idummy))
              (t nil)
            )
          )
        )
        (
          append
          (list
            '(mplus) 0
            (list
              '(mtimes)                ; e([...],[...],%1)*v([],[%1])
              (list (list v) '((mlist)) (list '(mlist) dummy))
              ($idiff e dummy)
            )
          )
          (maplist
            #'(lambda (s)              ; e([..%1..],[...])*v([],[%1],k)
              (list
                '(mtimes)
                (cond ((atom (car s)) 1) (t -1))
                (append
                  (list
                    (car e)
                    (cons
                      '(mlist)
                      (append
                        (subseq (cdadr e) 0 (- (length (cdadr e)) (length s)))
                        (cons
                          (cond ((atom (car s)) dummy)
                                (t (list '(mtimes simp) -1 dummy))
                          )
                          (cdr s)
                        )
                      )
                    )
                    (caddr e)
                  )
                  (cdddr e)
                )
                (vecdiff
                  v
                  (cond ((atom (car s))  dummy) (t (caddr (car s))))
                  (cond ((atom (car s)) (car s)) (t dummy))
                  dummy2
                )
              )
            )
            (cdadr e)
          )
          (maplist
            #'(lambda (s)              ; +e([...],[...],..%1..)*v([],[%1],k)
              (list
                '(mtimes)
                (append
                  (list (car e) (cadr e) (caddr e))
                  (subseq (cdddr e) 0 (- (length (cdddr e)) (length s)))
                  (cons dummy (cdr s))
                )
                (vecdiff v dummy (car s) dummy2)
              )
            )
            (cdddr e)
          )
          (maplist
            #'(lambda (s)             ; -e([...],[..%1..])*v([],[k],%1)
              (list
                '(mtimes) -1
                (append
                  (list (car e) (cadr e)
                    (cons
                      '(mlist)
                      (append
                        (subseq (cdaddr e) 0 (- (length (cdaddr e)) (length s)))
                        (cons dummy (cdr s))
                      )
                    )
                  )
                  (cdddr e)
                )
                (vecdiff v (car s) dummy dummy2)
              )
            )
            (cdaddr e)
          )
        )
      )
    )
    (
      (eq (caar e) 'mtimes)           ; Leibnitz rule
                                      ; Lv(cadr e)*(cddr e)+(cadr e)*Lv(cddr e)
      (list
        '(mplus)
        (cons '(mtimes) (cons (liediff v (cadr e) n) (cddr e)))
        (cons
          '(mtimes)
          (list
            (cadr e)
            (liediff
              v
              (cond ((cdddr e) (cons '(mtimes) (cddr e))) (t (caddr e)))
              n
            )
          )
        )
      )
    )
    (
      (eq (caar e) 'mplus)            ; Linearity
;     We prefer mapcar to iteration, but the recursive code also works
;     (list
;       '(mplus)
;       (liediff v (cadr e) n)
;       (liediff v (cond ((cdddr e) (cons '(mplus) (cddr e))) (t (caddr e))) n)
;     )
      (cons '(mplus) (mapcar #'(lambda (u) (liediff v u n)) (cdr e)))
    )
    (t (merror "~M is not a tensorial expression liediff can handle" e))
  )
)

(defmfun $liediff (v e) (liediff v e 1))

(defmfun $rediff (x) (meval '(($ev) x $idiff)))

;;(defmfun $evundiff (x) ($rediff ($undiff x)))
(defmfun $evundiff (x) (meval (list '($ev) ($undiff x) '$nouns)))

(defmfun $undiff (x) 
  (cond
    ((atom x) x)
    (
      (rpobj x)
      (cond
        (
          (cdddr x)
          (nconc
            (list '(%idiff) (list (car x) (cadr x) (caddr x)))
            (putinones (cdddr x))
          )
        )
        (t x)
      )
    )
    (t
      (mysubst0
        (simplifya (cons (ncons (caar x)) (mapcar '$undiff (cdr x))) t)
        x
      )
    )
  )
)

(defun putinones (e) 
  (cond
    ((cdr e) (cons (car e) (cons 1. (putinones (cdr e)))))
    (t (list (car e) 1.))
  )
) 



(defmfun $lorentz_gauge n
       (cond ((equal n 0) (merror "LORENTZ_GAUGE requires at least one argument"))
	     ((equal n 1) (lorentz (arg 1) nil))
	     (t (lorentz (arg 1)
			 ((lambda (l) (cond ((loop for v in  l
						     always (symbolp v)) l)
					    (t (merror
"Invalid tensor name(s) in argument to LORENTZ_GAUGE"))))
			  (listify (f- 1 n)))))))

;Lorentz contraction of E: indexed objects with a derivative index matching a
;contravariant index become 0. If L is NIL then do this for all indexed objects
;otherwise do this only for those indexed objects whose names are members of L.

(defun lorentz (e l)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((and (or (null l) (member (caar e) l :test #'eq))
			  (intersect (cdaddr e) (cdddr e)))
		     0.)
		    (t e)))
	     (t (mysubst0
		 (simplifya
		  (cons (ncons (caar e))
			(mapcar (function (lambda (q) (lorentz q l)))
				(cdr e)))
		  t) e))))

(defun less (x y)                                         ;alphanumeric compare
       (cond ((numberp x)
	      (cond ((numberp y) (< x y))
		    (t (alphalessp (ascii x) y))))
	     (t (cond ((numberp y) (alphalessp x (ascii y)))
		      (t (alphalessp x y)))))) 

;; Christoffels contains all Christoffel-like symbols: i.e., symbols
;; that make sense only with certain index patterns. These symbols are
;; excluded from contractions, because those would produce illegal
;; index combinations (e.g., ichr1([a,b],[c])). However, special rules
;; exist to convert a covariant symbol into a mixed symbol and vice
;; versa; for instance, g^ad*ichr1_bcd will contract to ichr2_bc^a.
(declare-top (special christoffels christoffels1 christoffels2))

(setq christoffels1 '($ichr1 %ichr1 $icc1 %icc1 $ifc1 %ifc1
                      $inmc1 %inmc1 $ikt1 %ikt1))
(setq christoffels2 '($ichr2 %ichr2 $icc2 %icc2 $ifc2 %ifc2
                      $inmc2 %inmc2 $ikt2 %ikt2))
(setq christoffels (append christoffels1 christoffels2 '(%ifb $ifb)))

;; Main contraction function
(defmfun $contract (e)
  (cond
    ((atom e) e)
    ((rpobj e) (contract5 e))
    (
      (eq (caar e) 'mtimes)
      (mysubst0 (simplifya (cons '(mtimes) (contract4a e)) nil) e)
    )
    (
      (eq (caar e) 'mplus)
      (mysubst0 (simplus (cons '(mplus) (mapcar '$contract (cdr e))) 1. t) e)
    )
    (t
      (mysubst0 (simplifya (cons (car e) (mapcar '$contract (cdr e))) nil) e)
    )
  )
)

(defun contract4a (e)
  (prog (l1 l2)
    (setq l1 nil l2 nil)
    (dolist (o (cdr e))
      (cond
        ((or (atom o) (atom (car o))) (setq l1 (cons o l1)))
        (
          (and (eq (caar o) 'mexpt) (eq (caddr o) -1))
          (setq l2 (cons (cadr o) l2))
        )
        (t (setq l1 (cons o l1)))
      )
    )
    (cond (l1 (setq l1 (contract4 (cons '(mtimes) l1)))))
    (cond (l2 (setq l1 (cons (list '(mexpt)
                                   (cons '(mtimes)
                                          (contract4 (cons '(mtimes) l2))
                                   )
                                   '-1
                             )
                             l1
                       ))))
    (return l1)
  )
)

;; Contract a single tensor with itself
(defun contract5 (e)
  (prog
    (       ; See if e contracts with itself, find contraction symbol
      (c (or (and (rpobj e) (getcon (caar e))) (return e)))
      (
        symbol
        (do
          (
            (c (getcon (caar e)) (cdr c))
          )
          ((or (eq (caar c) (caar e)) (null c)) (cond (c (cdar c)) (t nil)) )
        )
      )
    )
    (return
      (cond
        ((or (null symbol) (member (caar e) christoffels :test #'eq)) e)
        (
          t
          (prog (cov con f sgn)
            (setq sgn (cond ((rpobj ($canform e)) 1) (t -1))
                  cov (contractinside (derat (cadr e)))
                  con (derat (caddr e))
                  f (not (equal cov (derat (cadr e))))
            )
            ; Calling contract2 here won't do the trick as it messes up the
            ; order of indices. So we remove indices that appear both in cov
            ; and in con the hard way, with a do loop.
            (do
              ((i cov (cdr i)))
              ((null i))
              (cond
                ((not (atom (car i))))
                (
                  (member (car i) con)
                  (setq f t con (delete (car i) con) cov (delete (car i) cov))
                )
              )
            )
            (setq c
              (nconc
                (list (cond (f (list symbol)) (t (car e))) cov con)
                (cdddr e)
              )
            )
            (return (cond ((and f (eq sgn -1)) (list '(mtimes) sgn c)) (t c)))
          )
        )
      )
    )
  )
)

(defun head (x) (cond ((atom x) nil) (t (cons (car x) nil))))

(defun firstintersect (l1 l2) (head (intersect l1 l2)))

;; Remove like members. Return (cons l1 l2) or nil if no like members found.
(defun contract2 (l1 l2)
  (
    (lambda (i) (and i (cons (setdiff l1 i) (setdiff l2 i))))
    (firstintersect l1 l2)
  )
)

;; Return a list with those members of s1 that are not in s2
(defun setdiff (s1 s2)
  (do
    ((j s1 (cdr j)) (a))
    ((null j) (reverse a))
    (or
      (and (not (numberp (car j))) (member (car j) s2 :test #'eq))
      (setq a (cons (car j) a))
    )
  )
)

(defun contract3 (it lst)      ;Tries to contract IT with some element of LST.
       (prog (frst r rest)     ;If none occurs then return NIL otherwise return
			       ;a list whose first member is the result of
			       ;contraction and whose cdr is a top-level copy
		               ;of LST with the element which contracted
			       ;removed.
	loop (setq frst (car lst) lst (cdr lst))
;;	     (and (eq (caar frst) '%kdelta) (go skip))
	     (and (setq r (contract1 it frst))
		  (return (cons r (nconc (nreverse rest) lst))))
			       ;Try contraction in reverse order since the
			       ;operation is commutative.
;;	skip (and (zl-get (caar frst) 'contractions)
	skip (and (getcon (caar frst))
		  (setq r (contract1 frst it))
		  (return (cons r (nconc (nreverse rest) lst))))
	     (and (null lst) (return nil))
	     (setq rest (cons frst rest))
	     (go loop))) 

(defun contract4 (l)                                        ;contracts products
       (prog (l1 l2 l3 f cl sf)
	     (setq cl (cdr l)) ;Following loop sets up 3 lists from the factors
		               ;on L: L1 - atoms or the contraction of non
		               ;indexed objects (the contraction is to handle
			       ;sub-expressions in case E is not fully expanded
			       ;as in A*B*(C*D+E*F). ), L2 - indexed objects in
	                       ;L with contraction property, L3 - indexed
                               ;objects in L without contraction property
	again(setq f (car cl) cl (cdr cl))
	     (cond ((atom f) (setq l1 (cons f l1)))
		   ((rpobj f)
;;*** contract5 may return a negative result
		    (setq f (contract5 f))
(cond (
 (and (or (eq (car f) '(mtimes)) (eq (car f) '(mtimes simp))) (eq (cadr f) -1))
 (setq l1 (cons -1 l1) f (caddr f)) ))
		    (cond ((getcon (caar f))
			   (setq l2 (cons f l2)))
			  (t (setq l3 (cons f l3)))))
		   (t (setq l1 (cons ($contract f) l1))))
	     (and cl (go again))
	     (and (null l2) (return (nconc l1 l3)))
	     (and (null (cdr l2)) (setq cl l2) (go loop2+1))
                               ;If L2 is empty then no more contractions are
                               ;needed. If L2 has only 1 member then just
	                       ;contract it with L3 otherwise contract the
		               ;members of L2 with themselves. The following
		               ;loop goes down L2 trying to contract members
		               ;with other members according to the following
		               ;method: moving from front to end take current
	                       ;member (F) and see if it contracts with any
		               ;elements in the rest of the list (this is done
		               ;by CONTRACT3). If it doesn't then add it to CL.
		               ;If it does then take result of contraction and
			       ;add to L1, L2, or L3 as above.
	loop1(setq f (car l2) l2 (cdr l2))
	     (cond ((null (setq sf (contract3 f l2)))
		    (setq cl (cons f cl)))
		   (t
;;*** contract3 may also return a negative result
(setq sf (mapcar #'(lambda (x)
(cond ((atom x) x) (
 (and (or (equal (car x) '(mtimes)) (equal (car x) '(mtimes simp))) (eq (cadr x) -1))
 (setq l1 (cons -1 l1)) (caddr x)) (t x))
) sf ) )

 (setq l2 (cdr sf) sf (car sf))
		      (cond ((atom sf) (setq l1 (cons sf l1)))
			    ((rpobj sf)
;;			     (cond ((zl-get (caar sf)
;;					 'contractions)
			     (cond ((getcon (caar sf))
				    (setq l2 (cons sf l2)))
				   (t (setq l3 (cons sf l3)))))
			    (t (setq l1 (cons sf l1))))))
			       ;If L2 has at least 2 elements left then
		               ;continue loop. If L2 has 1 element and CL
			       ;is not empty and there were some contractions
			       ;performed last time then add CL to L2 and try
	                       ;again. Otherwise add L2 to CL and quit.
	     (and l2
		  (cond ((cdr l2) (go loop1))
			((and cl sf)
			 (setq sf nil l2 (cons (car l2) cl) cl nil)
			 (go loop1))
			(t (setq cl (nconc l2 cl)))))
			       ;The following loop goes down CL trying to
	                       ;contract each member with some member in L3. If
		               ;there is not a contraction then the element
			       ;from CL is added onto L3 (this causes elements
	                       ;of CL to be contracted with each other). If
	                       ;there is a contraction then the result is added
			       ;onto L3 by setting L3 to the result of
			       ;CONTRACT3 here if CL is known not to be null.
			       ;If L3 is empty then there is nothing left to
			       ;contract.
	loop2(and (null cl) (return (nconc l1 l3)))
	loop2+1
	     (and (null l3) (return (nconc l1 cl)))
	     (setq f (car cl) cl (cdr cl))
	     (cond ((setq sf (contract3 f l3))
;;*** contract3 may also return a negative result
(setq sf (mapcar #'(lambda (x)
(cond ((atom x) x) (
 (and (or (equal (car x) '(mtimes)) (equal (car x) '(mtimes simp))) (eq (cadr x) -1))
 (setq l1 (cons -1 l1)) (caddr x)) (t x))
) sf ) )

 (setq l3 sf))
		   (t (setq l3 (cons f l3))))
	     (go loop2))) 

;; Create a 'normalized' (i.e., old-style) rpobj
(defmfun $renorm (e &optional (force nil))
  (prog (c v)
    (and (not (rpobj e)) (merror "Not an RPOBJ: ~M" e))
    (and $allsym (setq force t))
    (setq c (cdaddr e) v nil)
    (do
      ((i (reverse (cdadr e)) (cdr i)))
      (
        (or (null i) (and (atom (car i)) (not force))) ; Terminating condition
        (setq v (append (reverse i) v))          ; Remaining covariant indices
      )
      (cond
        ((atom (car i)) (setq v (cons (car i) v)))
        (t (setq c (cons (caddar i) c)))
      )
    )
    (return
      (cons (car e) (append (list (cons smlist v) (cons smlist c)) (cdddr e)))
    )
  )
)

;; As above, but unconditionally. Not needed.
;(defun renorm (e) (append (list (car e) ($covi e) ($conti e)) (cdddr e)))

;; Add a minus sign to all elements in a list
(defun neglist (l)
  (cond ((null l) nil)
        (t (cons (list '(mtimes simp) -1 (car l)) (neglist (cdr l))))
  )
)

;; Create an 'abnormal' (i.e., new-style) rpobj
(defun abnorm (e)
  (append (list (car e)
                (append ($covi e) (neglist (conti e)))
                '((mlist simp)))
                (cdddr e)
  )
)

;; Substitute using EQUAL, to catch member lists
(defun substlist (b a l)
  (cond ((null l) l)
        ((equal a (car l)) (cons b (cdr l)))
        (t (cons (car l) (substlist b a (cdr l))))
  )
)

;; Removes items not in i from l.
(defun removenotin (i l)
  (cond ((null l) l)
        ((member (car l) i :test #'eq) (cons (car l) (removenotin i (cdr l))))
        (t (removenotin i (cdr l)))
  )
)

;; Removes items not in i from l. But the ones in l have a minus sign!
(defun removenotinm (i l)
  (cond ((null l) l)
        ((atom (car l)) (cons (car l) (removenotinm i (cdr l))))
        ((and (isprod (caar l)) (eq (cadar l) -1)
             (not (member (caddar l) i :test #'eq))) (removenotinm i (cdr l)))
        (t (cons (car l) (removenotinm i (cdr l))))
  )
)

;; Removes indices duplicated once with and once without a minus sign
(defun contractinside (c)
  (do
    ((i (minusi c) (cdr i)))
    ((null i))
    (and (member (car i) c :test #'equal)
	 (member (list '(mtimes simp) -1 (car i)) c :test #'equal)
         (setq c (delete (car i) (delete (list '(mtimes simp) -1 (car i)) c :test #'equal)))
    )
  )
  c
)

;; This does the actual contraction of f with g. If f has any derivative
;; indices then it can't contract g. If f is Kronecker delta then see which of
;; the covariant, contravariant, or derivative indices matches those in g.
(defun contract1 (f g)
  (prog (a b c d e cf sgn)
    (when (cdddr f) (return nil))
    (setq a (copy-tree (derat (cdadr f))) b (copy-tree (cdaddr f))
          c (copy-tree (derat (cadr g))) d (copy-tree (caddr g)) e (copy-tree (cdddr g))
    )
    (cond                        ; This section is all Kronecker-delta code
      (
        (or (eq (caar f) '%kdelta) (eq (caar f) '$kdelta))

        ; We normalize the indices first
        (setq b (append (minusi a) b) a (plusi a))

        ;We cannot contract with higher-order or malformed Kronecker deltas
        (and (or (/= (length a) 1) (/= (length b) 1 )) (return nil))

        (setq a (car a) b (car b))
        (return
          (simplifya
            (cond
              (
                (and (cdr c) (not (numberp b)) (member b (cdr c) :test #'eq))
                (setq c (subst a b (cdr c)))
                (and
                  (not (member (caar g) christoffels :test #'eq))
                  (cdr d)
                  (setq a (contract2 c (cdr d)))
                  (setq c (car a) d (cons smlist (cdr a)))
                )
                (setq c (contractinside c))
                (nconc (list (car g) (cons smlist c) d) e)
              )
              (
                (and e (not (numberp b)) (member b e :test #'eq))
                (nconc (list (car g) c d) 
                  (cond
                    ($iframe_flag (subst a b e))
                    (t (itensor-sort (subst a b e)))
                  )
                )
              )
              (
                (and (cdr d) (not (numberp a)) (member a (cdr d) :test #'eq))
                (setq d (subst b a (cdr d)))
                (and
                  (cdr c)
                  (setq a (contract2 (cdr c) d))
                  (setq d (cdr a) c (cons smlist (car a)))
                )
                (nconc (list (car g) c (cons smlist d)) e)
              )
              (
                (and (cdr c) (not (numberp a))
                     (member (list '(mtimes simp) -1 a) (cdr c) :test #'equal)
                )
                (setq c (substlist (list '(mtimes simp) -1 b)
                                   (list '(mtimes simp) -1 a)
                                   (cdr c)
                        )
                )
                (setq c (contractinside c))
                (nconc (list (car g) (cons smlist c) d) e)
              )
              (t nil)
            )
            nil
          )
        )
      )
    )

    ;No tensor can contract Kronecker-deltas or Levi-Civita symbols.
    (and
      (or (eq (caar g) '$kdelta) (eq (caar g) '%kdelta)
          (eq (caar g) '$levi_civita) (eq (caar g) '%levi_civita)
          (eq (caar g) '$icurvature) (eq (caar g) '%icurvature)
      )
      (return nil)
    )

    ;If g has derivative indices then F must be constant in order to contract it
    (and e (not (kindp (caar f) '$constant)) (return nil))

    ;Contraction property of f is a list of (a.b)'s
    (cond
      ((setq cf (getcon (caar f))))
      (t (return nil))
    )

    ; Determine the sign of the result based on the expression's symmetry
    ; properties. We use CANFORM to sort indices in the canonical order
    ; and then extract the resulting expression's sign.
    (setq sgn
      (cond ((eq -1 (cadr ($canform (list '(mtimes simp) f g)))) -1) (t 1))
    )

    ;If g matches an a then use the b for name of result. If an a is a space
    ;use name of G for result.
    more
    (cond
      (
        (eq (caar cf) '/ )
        (setq cf (car g))
      )
      (
        (eq (caar cf) (caar g))
        (setq cf (ncons (cdar cf)))
      )
      (t
        (or (setq cf (cdr cf)) (return nil))
        (go more)
      )
    )
    (setq c (cdr c) d (cdr d))

    ;If CONTRACT2 of f's contravariant and g's covariant or f's covariant and
    ;g's contravariant indices is nil then return nil
    (cond
      (
        (and b c (setq f (contract2 b c)))
        (setq b (car f) c (cdr f))
      )
      (
        (and a d (setq f (contract2 a d)))
        (setq a (car f) d (cdr f))
      )
      (
        (and a (minusi c) (setq f (contract2 a (minusi c))))
        ; (cdr f) now contains the free indices in (minusi c).
        ; what we need to do is find the corresponding items in c, and remove
        ; all other negative indices (i.e., those that were dropped by
        ; contract2).
        ; What we need to do is remove items from c one by one, and substitute
        ; an item from (car f), which we should remove from (car f):
        ; for i thru length(c)
        ;    if c[i] not in (cdr f)
        ;       if (car f) is nil, remove c[i]
        ;       otherwise subst c[i]
        ; endfor
        ; Now set c to what we made of c, a to whatever is left of (cdr f)

        (do
          (
            (i c (cdr i))
            (j (car f))
            (k)
          )
          ((null i) (setq a (removenotin j a) c (reverse k)))
          (cond
            (
              (or (atom (car i)) (member (caddar i) (cdr f)))
              (setq k (cons (car i) k))
            )
            (
              (not (null j))
              (setq k (cons (car j) k) j (cdr j))
            )
          )
        )
      )
      (
        (and (minusi a) c (setq f (contract2 (minusi a) c)))
        (do
          (
            (i c (cdr i))
            (j (car f))
            (k)
          )
;;          ((null i) (setq c (reverse k) a (append (plusi a) j)))
          ((null i)
            (setq
              c (reverse k)
              a (append
                (plusi a)
                (mapcar #'(lambda (x) (list '(mtimes simp) -1 x)) j)
              )
            )
          )
          (cond
            ((member (car i) (cdr f)) (setq k (cons (car i) k)))
            (
              (not (null j))
              (setq k (cons (list '(mtimes simp) -1 (car j)) k) j (cdr j))
            )
          )
        )
      )
      (t (return nil))
    )
    ;Form combined indices of result
    (and d (setq b (append b d)))
    (and c (setq a (append c a)))
    ;Zl-remove repeated indices
;;    (and (setq f (contract2 a b)) (setq a (car f) b (cdr f)))
;;    (setq a (contractinside a))

    ;VTT: Special handling of Christoffel symbols. We can only contract them
    ;when we turn ICHR1 into ICHR2 or vice versa; other index combinations are
    ;illegal. This code checks if the index pattern is a valid one and replaces
    ;ICHR1 with ICHR2 or vice versa as appropriate.
    (cond
      (
        (member (car cf) christoffels1)
        (cond
          (
            ;;(and (eq (length a) 2) (eq (length b) 1))
            (and (eq (+ (length (plusi a)) (length (minusi b))) 2) (eq (+ (length (plusi b)) (length (minusi a))) 1))
            (setq cf
              (cons
                (elt christoffels2 (position (car cf) christoffels1))
                (cdr cf)
              )
            )
          )
          (
            ;; (not (and (eq (length a) 3) (eq (length b) 0)))
            (not (and (eq (+ (length (plusi a)) (length (minusi b))) 3) (eq (+ (length (plusi b)) (length (minusi a))) 0)))
            (return nil)
          )
        )
      )
      (
        (member (car cf) christoffels2)
        (cond
          (
            ;;(and (eq (length a) 3) (eq (length b) 0))
            (and (eq (+ (length (plusi a)) (length (minusi b))) 3) (eq (+ (length (plusi b)) (length (minusi a))) 0))
            (setq cf
              (cons
                (elt christoffels1 (position (car cf) christoffels2))
                (cdr cf)
              )
            )
          )
          (
            ;;(not (and (eq (length a) 2) (eq (length b) 1)))
            (not (and (eq (+ (length (plusi a)) (length (minusi b))) 2) (eq (+ (length (plusi b)) (length (minusi a))) 1)))
            (return nil)
          )
        )
      )
      ((member (car cf) christoffels) (return nil))
    )

    (setq f (meval (list cf (cons smlist a) (cons smlist b))))
    (and e
      (do
        ((e e (cdr e)))
        ((null e))
        (setq f (idiff f (car e)))
      )
    )
    (return (cond ((eq sgn -1) (list '(mtimes) sgn f)) (t f)))
  )
)

;; In what amounts to quite an abuse of the Kronecker delta concept, we
;; permit an exceptional index combination of two contravariant indices.
;; This helps lc2kdt convert Levi-Civita symbols in a manner that does
;; not require resorting to numeric indices, causing all sorts of problems
;; with RENAME and CONTRACT.
(defmfun $kdelta (l1 l2)
  (setq l2 (append l2 (minusi l1)) l1 (plusi l1))
  (cond
    (
      (and ($listp l1) ($listp l2) (= ($length l1) 0) (= ($length l2) 2))
      (cond
        ((eq (cadr l2) (caddr l2)) 1)
        (
          (and (numberp (cadr l2)) (numberp (caddr l2)))
          (cond
            ((= (cadr l2) (caddr l2)) t)
            (t 0)
          )
        )
        (t (list '(%kdelta) l1 l2))
      )
    )
    (
      (and ($listp l1) ($listp l2) (= ($length l1) 2) (= ($length l2) 0))
      (cond
        ((eq (cadr l1) (caddr l1)) 1)
        (
          (and (numberp (cadr l1)) (numberp (caddr l1)))
          (cond
            ((= (cadr l1) (caddr l1)) t)
            (t 0)
          )
        )
        (t (list '(%kdelta) l1 l2))
      )
    )
    (
      (null (and ($listp l1) ($listp l2) (= (length l1) (length l2))))
      (merror "Improper arg to DELTA: ~M" (list '(%kdelta) l1 l2))
    )
    (t (delta (cdr l1) (cdr l2)))
  )
)

;kdels defines the symmetric combination of the Kronecker symbols

(defmfun $kdels (l1 l2)
       (cond ((null (and ($listp l1)
			 ($listp l2)
			 (= (length l1) (length l2))))
	      (merror "Improper arg to DELTA: ~M"
		      (list '(%kdels) l1 l2)
		      ))
	     (t (delta (cdr l1) (cdr l2) 1)))) 

(defun delta (lower upper &optional (eps -1))
  (cond ((null lower) $dim)
        ((null (cdr lower))
         (cond ((equal (car upper) (car lower))
                (cond ((numberp (car upper)) 1.) (t $dim)))
               ((and (numberp (car upper)) (numberp (car lower))) 0.)
               (t (list '(%kdelta) (cons smlist lower) (cons smlist upper)))))
        (t (do ((left nil (append left (ncons (car right))))
		(right lower (cdr right))
                (result))
               ((null right) (simplus (cons '(mplus) result) 1. t))
               (setq result (cons (simptimes
                                   (list '(mtimes) (delta (ncons (car right)) (ncons (car upper)) eps)
                                         (delta (append left (cdr right)) (cdr upper) eps)
                                         (cond ((oddp (length left)) eps) (t 1))
                                   ) 1. t
                                  ) result)
              )))))

(declare-top (special $outchar $dispflag *linelabel* foobar derivlist))


;Displays P([L1],[L2],I1,I2,...) by making the elements of L2 into a single
;atom which serves as the exponent and the elements of L1 and I1,I2,... into a
;single atom with a comma in between which serves as the subscript.

(defmfun $ishow (f)
       (progn (makelabel $linechar)
              (cond ($dispflag
                     (displa (list '(mlabel) *linelabel* (ishow (specrepcheck (derat f)))))
;                     (setq $dispflag nil)
))
              (set *linelabel* f)))

(defun ishow (f) 
       ((lambda (foobar)                              ;FOOBAR intialized to NIL
		(cond ((atom f) f)
		      ((rpobj f)                      ;If an indexed object ...
		       (setq foobar
			     (cond ((or (covi f) (cdddr f))   ;If covariant or
				    (cons (list (caar f)    ;derivative indices
						'array)
					  (ncons (maknam (cons '$ (splice (covi f)
							 (cdddr f)))))))
				   (t (caar f))))
		       (cond ((conti f)              ;If contravariant indices
			      (list '(mexpt simp)
				    foobar
;				     (cons '(mtimes simp)  ;Make indices appear
;					  (conti f))))    ;as exponents for
					(maknam (cons '$ (splice (conti f) nil)))))	; Changed for wxmaxima
			     (t foobar)))                  ;proper display
		      (t
		       (cons (car f) (mapcar 'ishow (cdr f))))))
	nil))                                           ;Map onto subparts of F

(defun splice (l1 l2) 
       (cond (l2 (setq l2 (cons '|,| (splice1 l2)))
		 (and l1 (setq l2 (nconc (splice1 l1) l2)))
		 l2)
	     (t (splice1 l1)))) 

(defun splice1 (l)
  (cond ((null (cdr l))(splice2 (car l)))
	(t (nconc (splice2 (car l))(cons '| | (splice1 (cdr l)))))))

(defun splice2 (x)
  (cond ((fixnump x)(explode x))
	(t (cdr (explodec x)))))
;	(t (cdr (explodec (print-invert-case x))))))

(defun deriv (e) 
       (prog (exp z count v) 
	     (cond ((null (cdr e)) (return (stotaldiff (car e))))
		   ((null (cddr e)) (nconc e '(1.))))
	     (setq exp (car e) z (setq e (append e nil)))
	loop (cond ((or (null derivlist) (member (cadr z) derivlist :test #'equal))
		    (go doit)))
						       ;DERIVLIST is set by $EV
	     (setq z (cdr z))
	loop2(cond ((cdr z) (go loop))
		   ((null (cdr e)) (return exp))
		   (t (go noun)))
	doit (cond ((null (cddr z))
		    (merror "Wrong number of args to DERIVATIVE"))
		   ((not (fixnump (setq count (caddr z)))) (go noun))
		   ((< count 0.)
		    (merror "Improper count to DIFF: ~M"
			    count)))
	loop1(setq v (cadr z))
	     (and (fixnump v)
		  $vect_coords
		  (> v 0.)
		  (not (> v $dim))
		  (setq v
			(cond ((atom $vect_coords)
			       (meval1 (list (list $vect_coords 'simp 'array)
					     v)))
			      ((eq (caar $vect_coords) 'mlist)
			       (cond ((not (< v
					      (length $vect_coords)))
				      (merror
"Coordinate list too short for derivative index"))
				     (t (nth v $vect_coords))))
			      (t v))))
	     (cond ((zerop count) (rplacd z (cdddr z)) (go loop2))
		   ((zerop1 (setq exp (sdiff exp v))) (return 0.)))
	     (setq count (1- count))
	     (go loop1)
	noun (return (diff%deriv (cons exp (cdr e))))))

(defun chainrule1 (e x)					; --ys 15.02.02
	(prog (y)
		(cond ((and (atom e) (eq (setq y (car (mget e 'depends)))
			(cadr $coord))) (return (subst x y (chainrule e y))))
		(t (return (chainrule e x))))))

(defun diffexpt1 (e x)
;; RETURN: n*v^n*rename(v'/v) where e=v^n
  (list '(mtimes) (caddr e) e
    ($rename
      (list '(mtimes) (list '(mexpt) (cadr e) -1)
             (sdiff (cadr e) x)
      )
    )
  )
)

;Redefined so that the derivative of any indexed object appends on the
;coordinate index in sorted order unless the indexed object was declared
;constant in which case 0 is returned.
(defun sdiff (e x) 
  (simplifya
       (cond ((mnump e) 0.)
	     ((and (alike1 e x) (not (and (rpobj e) (rpobj x)))) 1.)
	     ((or (atom e) (member 'array (cdar e) :test #'eq))
	      (chainrule1 e x))
	     ((kindp (caar e) '$constant) 0.)                    ;New line added
	     ((eq (caar e) 'mrat) (ratdx e x))
	     ((eq (caar e) 'mplus)
	      (simplus (cons '(mplus) (sdiffmap (cdr e) x))
		       1.
		       t))
	     ((eq (caar e) 'mequal)
	      (list (car e) (sdiff (cadr e) x) (sdiff (caddr e) x)))
	     ((eq (caar e) '$matrix)
	      (cons (car e)
		    (mapcar 
		     (function (lambda (y) 
				       (cons (car y)
					     (sdiffmap (cdr y) x))))
		     (cdr e))))
	     ((eq (caar e) 'mtimes)
 	      (addn (sdifftimes (cdr e) x) t))
	     ((eq (caar e) 'mexpt) (diffexpt e x))
;;	     ((rpobj e) (diffrpobj e x))                        ;New line added
;;	     ((and (boundp '$imetric) (eq (caar e) '%determinant);New line added
;;		   (eq (cadr e) $imetric))
;;	      ((lambda (dummy)
;;		       (setq dummy ($idummy))
;;		       (cond ((eq dummy x) (setq dummy ($idummy))))
;;		       (list '(mtimes simp) 2. e
;;			     (list '($ichr2 simp) (cons smlist (list dummy x))
;;				   (cons smlist (ncons dummy)))))
;;	       nil))

         ((and
              (boundp '$imetric)
              (rpobj x)
              (eq (caar e) '%determinant)
              (eq (cadr e) $imetric)
          )
          (cond
           ((and
             (eq (caar x) $imetric)
             (eq (length (cdadr x)) 0)
             (eq (length (cdaddr x)) 2)
             (eq (length (cdddr x)) 0)
            )
            (list '(mtimes simp)
                   -1
                  (list '(%determinant simp) $imetric)
                  (list (cons $imetric '(simp))
                        (list '(mlist simp) (nth 0 (cdaddr x)) (nth 1 (cdaddr x)))
                        '((mlist simp))
                  )
            )
           )
           ((and
             (eq (caar x) $imetric)
             (eq (length (cdadr x)) 2)
             (eq (length (cdaddr x)) 0)
             (eq (length (cdddr x)) 0)
            )
            (list '(mtimes simp)
                  (list '(%determinant simp) $imetric)
                  (list (cons $imetric '(simp))
                        '((mlist simp))
                        (list '(mlist simp) (nth 0 (cdadr x)) (nth 1 (cdadr x)))
                  )
            )
           )
           (t 0.)
          )
         )


         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; Differentiation of tensors with respect to tensors ;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;
         ((and (rpobj e) (rpobj x)) ; (merror "Not yet..."))
          (cond

            ( ;; dg([a,b],[])/dg([],[m,n])
             (and
              (boundp '$imetric)
              (eq (caar e) $imetric)
              (eq (caar x) $imetric)
              (eq (length (cdadr e)) 2)
              (eq (length (cdaddr e)) 0)
              (eq (length (cdddr e)) 0)
              (eq (length (cdadr x)) 0)
              (eq (length (cdaddr x)) 2)
              (eq (length (cdddr x)) 0)
             )
             (list '(mtimes simp)
                   -1
                   (list
                    (cons $imetric '(simp))
                    (list '(mlist simp) (nth 0 (cdadr e)) (nth 0 (cdaddr x)))
                    '((mlist simp))
                   )
                   (list
                    (cons $imetric '(simp))
                    (list '(mlist simp) (nth 1 (cdadr e)) (nth 1 (cdaddr x)))
                    '((mlist simp))
                   )
             )
            )

            ( ;; dg([],[a,b])/dg([m,n],[])
             (and
              (boundp '$imetric)
              (eq (caar e) $imetric)
              (eq (caar x) $imetric)
              (eq (length (cdadr e)) 0)
              (eq (length (cdaddr e)) 2)
              (eq (length (cdddr e)) 0)
              (eq (length (cdadr x)) 2)
              (eq (length (cdaddr x)) 0)
              (eq (length (cdddr x)) 0)
             )
             (list '(mtimes simp)
                   -1
                   (list
                    (cons $imetric '(simp))
                    '((mlist simp))
                    (list '(mlist simp) (nth 0 (cdaddr e)) (nth 0 (cdadr x)))
                   )
                   (list
                    (cons $imetric '(simp))
                    '((mlist simp))
                    (list '(mlist simp) (nth 1 (cdaddr e)) (nth 1 (cdadr x)))
                   )
             )
            )

            ( ;; dg([a,b],[],y)/dg([],[m,n])
             (and
              (boundp '$imetric)
              (eq (caar e) $imetric)
              (eq (caar x) $imetric)
              (eq (length (cdadr e)) 2)
              (eq (length (cdaddr e)) 0)
              (eq (length (cdddr e)) 1)
              (eq (length (cdadr x)) 0)
              (eq (length (cdaddr x)) 2)
              (eq (length (cdddr x)) 0)
             )
             (prog (d1 d2)
              (setq d1 ($idummy) d2 ($idummy))
              (return
               (list '(mtimes simp)
                   (list
                    (cons $imetric '(simp))
                    '((mlist simp))
                    (list '(mlist simp) d1 d2)
                    (cadddr e)
                   )
                   (list
                     '(mplus simp)
                     (list
                       '(mtimes simp)
                       (list
                        (cons $imetric '(simp))
                        (list
                          '(mlist simp)
                          (nth 0 (cdadr e))
                          (nth 0 (cdaddr x))
                        )
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) d1 (nth 1 (cdaddr x)))
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) (nth 1 (cdadr e)) d2)
                        '((mlist simp))
                       )
                     )
                     (list
                       '(mtimes simp)
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) (nth 0 (cdadr e)) d1)
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list
                          '(mlist simp)
                          (nth 1 (cdadr e))
                          (nth 0 (cdaddr x))
                        )
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) d2 (nth 1 (cdaddr x)))
                        '((mlist simp))
                       )
                     )
                   )
               )
              )
             )
            )

            ( ;; dg([a,b],[],y)/dg([],[m,n],k)
             (and
              (boundp '$imetric)
              (eq (caar e) $imetric)
              (eq (caar x) $imetric)
              (eq (length (cdadr e)) 2)
              (eq (length (cdaddr e)) 0)
              (eq (length (cdddr e)) 1)
              (eq (length (cdadr x)) 0)
              (eq (length (cdaddr x)) 2)
              (eq (length (cdddr x)) 1)
             )
             (list '(mtimes simp)
                   -1
                   (list
                    (cons $imetric '(simp))
                    (list '(mlist simp) (nth 0 (cdadr e)) (nth 0 (cdaddr x)))
                    '((mlist simp))
                   )
                   (list
                    (cons $imetric '(simp))
                    (list '(mlist simp) (nth 1 (cdadr e)) (nth 1 (cdaddr x)))
                    '((mlist simp))
                   )
                   (list
                    '(%kdelta simp)
                     (list '(mlist simp) (cadddr e))
                     (list '(mlist simp) (cadddr x))
                   )
             )
            )

            ( ;; dg([a,b],[],y,d)/dg([],[m,n])
             (and
              (boundp '$imetric)
              (eq (caar e) $imetric)
              (eq (caar x) $imetric)
              (eq (length (cdadr e)) 2)
              (eq (length (cdaddr e)) 0)
              (eq (length (cdddr e)) 2)
              (eq (length (cdadr x)) 0)
              (eq (length (cdaddr x)) 2)
              (eq (length (cdddr x)) 0)
             )
             (prog (d1 d2)
              (setq d1 ($idummy) d2 ($idummy))
              (return
               (list '(mtimes simp)
                   (list
                    (cons $imetric '(simp))
                    '((mlist simp))
                    (list '(mlist simp) d1 d2)
                    (nth 0 (cdddr e))
                    (nth 1 (cdddr e))
                   )
                   (list
                     '(mplus simp)
                     (list
                       '(mtimes simp)
                       (list
                        (cons $imetric '(simp))
                        (list
                          '(mlist simp)
                          (nth 0 (cdadr e))
                          (nth 0 (cdaddr x))
                        )
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) d1 (nth 1 (cdaddr x)))
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) (nth 1 (cdadr e)) d2)
                        '((mlist simp))
                       )
                     )
                     (list
                       '(mtimes simp)
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) (nth 0 (cdadr e)) d1)
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list
                          '(mlist simp)
                          (nth 1 (cdadr e))
                          (nth 0 (cdaddr x))
                        )
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) d2 (nth 1 (cdaddr x)))
                        '((mlist simp))
                       )
                     )
                   )
               )
              )
             )
            )

            ( ;; dg([a,b],[],y,d)/dg([],[m,n],k)
             (and
              (boundp '$imetric)
              (eq (caar e) $imetric)
              (eq (caar x) $imetric)
              (eq (length (cdadr e)) 2)
              (eq (length (cdaddr e)) 0)
              (eq (length (cdddr e)) 2)
              (eq (length (cdadr x)) 0)
              (eq (length (cdaddr x)) 2)
              (eq (length (cdddr x)) 1)
             )
             (prog (d1 d2 d3 d4)
              (setq d1 ($idummy) d2 ($idummy) d3 ($idummy) d4 ($idummy))
              (return
               (list
                '(mtimes simp)
                (list
                 '(mplus simp)
                 (list
                  '(mtimes simp)
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) (nth 0 (cdadr e)) d3)
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) d2 d4)
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) (nth 1 (cdadr e)) d1)
                        '((mlist simp))
                       )
                 )
                 (list
                  '(mtimes simp)
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) (nth 0 (cdadr e)) d2)
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) (nth 1 (cdadr e)) d3)
                        '((mlist simp))
                       )
                       (list
                        (cons $imetric '(simp))
                        (list '(mlist simp) d1 d4)
                        '((mlist simp))
                       )
                 )
                )
                (list
                 '(mplus simp)
                 (list
                  '(mtimes simp)
                       (list
                        '(%kdelta simp)
                         (list '(mlist simp) (nth 0 (cdaddr x)))
                         (list '(mlist simp) d3)
                       )
                       (list
                        '(%kdelta simp)
                         (list '(mlist simp) (nth 1 (cdaddr x)))
                         (list '(mlist simp) d4)
                       )
                       (list
                        '(%kdelta simp)
                         (list '(mlist simp) (nth 1 (cdddr e)))
                         (list '(mlist simp) (nth 0 (cdddr x)))
                       )

                       (list
                        (cons $imetric '(simp))
                        '((mlist simp))
                        (list '(mlist simp) d2 d1)
                        (nth 0 (cdddr e))
                       )
                 )
                 (list
                  '(mtimes simp)
                       (list
                        '(%kdelta simp)
                         (list '(mlist simp) (nth 0 (cdaddr x)))
                         (list '(mlist simp) d2)
                       )
                       (list
                        '(%kdelta simp)
                         (list '(mlist simp) (nth 1 (cdaddr x)))
                         (list '(mlist simp) d1)
                       )
                       (list
                        '(%kdelta simp)
                         (list '(mlist simp) (nth 0 (cdddr e)))
                         (list '(mlist simp) (nth 0 (cdddr x)))
                       )

                       (list
                        (cons $imetric '(simp))
                        '((mlist simp))
                        (list '(mlist simp) d3 d4)
                        (nth 1 (cdddr e))
                       )
                 )
                )
               )
              )
             )
            )

            ( ;; dg([a,b],[],y,d)/dg([],[m,n],k,l)
             (and
              (boundp '$imetric)
              (eq (caar e) $imetric)
              (eq (caar x) $imetric)
              (eq (length (cdadr e)) 2)
              (eq (length (cdaddr e)) 0)
              (eq (length (cdddr e)) 2)
              (eq (length (cdadr x)) 0)
              (eq (length (cdaddr x)) 2)
              (eq (length (cdddr x)) 2)
             )
             (list '(mtimes simp)
                   -1
                   (list
                    (cons $imetric '(simp))
                    (list '(mlist simp) (nth 0 (cdadr e)) (nth 0 (cdaddr x)))
                    '((mlist simp))
                   )
                   (list
                    (cons $imetric '(simp))
                    (list '(mlist simp) (nth 1 (cdadr e)) (nth 1 (cdaddr x)))
                    '((mlist simp))
                   )
                   (list
                    '(%kdelta simp)
                     (list '(mlist simp) (cadddr e))
                     (list '(mlist simp) (cadddr x))
                   )
                   (list
                    '(%kdelta simp)
                     (list '(mlist simp) (nth 1 (cdddr e)))
                     (list '(mlist simp) (nth 1 (cdddr x)))
                   )
             )
            )


            ((and
               (eq (caar e) (caar x))
               (eq (length (cdadr e)) (length (cdadr x)))
               (eq (length (cdaddr e)) (length (cdaddr x)))
               (eq (length (cdddr e)) (length (cdddr x)))
             )
             (cons '(mtimes)
              (cons 1
               (append
                 (mapcar
                   #'(lambda (x y)
                       (list
                         '(%kdelta simp)
                         (list '(mlist simp) x)
                         (list '(mlist simp) y)
                       )
                     ) (cdadr e) (cdadr x)
                 )
                 (mapcar
                   #'(lambda (x y)
                       (list
                         '(%kdelta simp)
                         (list '(mlist simp) x)
                         (list '(mlist simp) y)
                       )
                     ) (cdaddr x) (cdaddr e)
                 )
                 (mapcar
                   #'(lambda (x y)
                       (list
                         '(%kdelta simp)
                         (list '(mlist simp) x)
                         (list '(mlist simp) y)
                       )
                     )
                     (cdddr e) (cdddr x)
                 )
               )
              )
             )
            )
            ((or
              (and ;; catchall symbols constructed from the metric tensor
               (boundp '$imetric)
               (eq (caar x) $imetric)
               (member
                 (caar e)
                 (cons '$icurvature (cons '%icurvature christoffels))
               )
              )
              (and ;; d(some covi)/d(cov metric)
               (boundp '$imetric)
               (not (eq (caar e) $imetric))
               (eq (caar x) $imetric)
               (eq (length (cdadr x)) 2)
               (eq (length (cdaddr x)) 0)
               (eq (length (cdddr x)) 0)
               (> (+ (length (cdadr e)) (length (cdddr e))) 0)
              )
              (and ;; d(some conti)/d(cont metric)
               (boundp '$imetric)
               (not (eq (caar e) $imetric))
               (eq (caar x) $imetric)
               (eq (length (cdadr x)) 0)
               (eq (length (cdaddr x)) 2)
               (eq (length (cdddr x)) 0)
               (> (length (cdaddr e)) 0)
              )
              (and ;; da([a,b],y)/da([m,n],k) with a+b=m+n, y=k
               (depends (caar e) (caar x))
               (eq (+ (length (cdadr e)) (length (cdaddr e)))
                   (+ (length (cdadr x)) (length (cdaddr x))))
               (eq (length (cdddr e)) (length (cdddr x)))
              )
             )
             (list '(%derivative) e x)
            )
            (t 0.)
          )
         )
         ;; End of tensor vs. tensor differentiation

	     ((not (depends e x))
	      (cond ((fixnump x) (list '(%derivative) e x))
		    ((atom x) 0.)
		    (t (list '(%derivative) e x))))
							  ;This line moved down
	     ((eq (caar e) 'mnctimes)
	      (simplus (list '(mplus)
			     (list '(mnctimes)
				   (sdiff (cadr e) x)
				   (caddr e))
			     (list '(mnctimes)
				   (cadr e)
				   (sdiff (caddr e) x)))
		       1.
		       nil))
	     ((eq (caar e) 'mncexpt) (diffncexpt e x))
	     ((eq (caar e) '%integrate) (diffint e x))
	     ((eq (caar e) '%derivative)
	      (cond ((or (atom (cadr e))
			 (member 'array (cdaadr e) :test #'eq))
		     (chainrule1 e x))
		    ((freel (cdr e) x) 0.)
		    (t (diff%deriv (list e x 1.)))))
	     ((member (caar e) '(%sum %product) :test #'eq) (diffsumprod e x))
	     (t (sdiffgrad e x)))
  t )
) 

; VTT: several of these functions have been copied verbatim from comm.lisp and
; comm2.lisp, in order to implement indicial differentiation as distinct from
; differentiation with respect to an external variable.

(defun idiffmap (e x) (mapcar #'(lambda (term) (idiff term x)) e))

(defun idifftimes (l x)
  (prog (term left out)
   loop (setq term (car l) l (cdr l))
   (setq out (cons (muln (cons (idiff term x) (append left l)) t) out))
   (if (null l) (return out))
   (setq left (cons term left))
   (go loop)))

(defun idiffexpt1 (e x)
;; RETURN: n*v^n*rename(v'/v) where e=v^n
  (list '(mtimes) (caddr e) e
;;    ($rename
      (list '(mtimes) (list '(mexpt) (cadr e) -1)
             (idiff (cadr e) x)
      )
;;    )
  )
)

(defun idiffexpt (e x)
  (if (mnump (caddr e))
      (mul3 (caddr e) (power (cadr e) (addk (caddr e) -1)) (idiff (cadr e) x))
      (mul2 e (add2 (mul3 (power (cadr e) -1) (caddr e) (idiff (cadr e) x))
            (mul2 (simplifya (list '(%log) (cadr e)) t)
              (idiff (caddr e) x))))))

(defmfun idiffint (e x)
  (let (a)
    (cond ((null (cdddr e))
       (cond ((alike1 x (caddr e)) (cadr e))
         ((and (not (atom (caddr e))) (atom x) (not (free (caddr e) x)))
          (mul2 (cadr e) (idiff (caddr e) x)))
         ((or ($constantp (setq a (idiff (cadr e) x)))
              (and (atom (caddr e)) (free a (caddr e))))
          (mul2 a (caddr e)))
         (t (simplifya (list '(%integrate) a (caddr e)) t))))
      ((alike1 x (caddr e)) (addn (idiffint1 (cdr e) x x) t))
      (t (addn (cons (if (equal (setq a (idiff (cadr e) x)) 0)
                 0
                 (simplifya (list '(%integrate) a (caddr e)
                          (cadddr e) (car (cddddr e)))
                    t))
             (idiffint1 (cdr e) x (caddr e)))
           t)))))

(defun idiffint1 (e x y)
  (let ((u (idiff (cadddr e) x)) (v (idiff (caddr e) x)))
    (list (if (pzerop u) 0 (mul2 u (maxima-substitute (cadddr e) y (car e))))
      (if (pzerop v) 0 (mul3 v (maxima-substitute (caddr e) y (car e)) -1)))))

(defun idiff%deriv (e)
  (declare (special derivflag))
  (let (derivflag) (simplifya (cons '(%idiff) e) t)))

(defun ideriv (e)
  (prog (exp z count)
     (cond ((null e) (wna-err '$idiff))
       ((null (cdr e)) (return (stotaldiff (car e))))
       ((null (cddr e)) (nconc e '(1))))
     (setq exp (car e) z (setq e (copy-list e)))
     loop (if (or (null derivlist) (member (cadr z) derivlist :test #'equal)) (go doit))
                    ; DERIVLIST is set by $EV
     (setq z (cdr z))
     loop2(cond ((cdr z) (go loop))
        ((null (cdr e)) (return exp))
        (t (go noun)))
     doit (cond ((nonvarcheck (cadr z) '$idiff))
        ((null (cddr z)) (wna-err '$idiff))
        ((not (eq (ml-typep (caddr z)) 'fixnum)) (go noun))
        ((minusp (setq count (caddr z)))
         (merror "Improper count to IDIFF:~%~M" count)))
     loop1(cond ((zerop count) (rplacd z (cdddr z)) (go loop2))
        ((equal (setq exp (idiff exp (cadr z))) 0) (return 0)))
     (setq count (f1- count))
     (go loop1)
     noun (return (idiff%deriv (cons exp (cdr e))))))


(defmfun idiffncexpt (e x)
  ((lambda (base* pow)
     (cond ((and (mnump pow) (or (not (eq (ml-typep pow) 'fixnum)) (< pow 0))) ; POW cannot be 0
        (idiff%deriv (list e x 1)))
       ((and (atom base*) (eq base* x) (free pow base*))
        (mul2* pow (list '(mncexpt) base* (add2 pow -1))))
       ((ml-typep pow 'fixnum)
        ((lambda (deriv ans)
           (do ((i 0 (f1+ i))) ((= i pow))
         (setq ans (cons (list '(mnctimes) (list '(mncexpt) base* i)
                       (list '(mnctimes) deriv
                         (list '(mncexpt) base* (f- pow 1 i))))
                 ans)))
           (addn ans nil))
         (idiff base* x) nil))
       ((and (not (depends pow x)) (or (atom pow) (and (atom base*) (free pow base*))))
        ((lambda (deriv index)
           (simplifya
        (list '(%sum)
              (list '(mnctimes) (list '(mncexpt) base* index)
                (list '(mnctimes) deriv
                  (list '(mncexpt) base*
                    (list '(mplus) pow -1 (list '(mtimes) -1 index)))))
              index 0 (list '(mplus) pow -1)) nil))
         (idiff base* x) (gensumindex)))
       (t (idiff%deriv (list e x 1)))))
   (cadr e) (caddr e)))

(defmfun idiffsumprod (e x)
  (cond ((or (not (atom x)) (not (free (cadddr e) x)) (not (free (car (cddddr e)) x)))
     (idiff%deriv (list e x 1)))
    ((eq (caddr e) x) 0)
    (t (let ((u (idiff (cadr e) x)))
         (setq u (simplifya (list '(%sum)
                      (if (eq (caar e) '%sum) u (div u (cadr e)))
                      (caddr e) (cadddr e) (car (cddddr e)))
                t))
         (if (eq (caar e) '%sum) u (mul2 e u))))))

(defun idiffgrad (e x)
  (let ((fun (caar e)) grad args)
    (cond ((and (eq fun 'mqapply) (oldget (caaadr e) 'grad))
       (idiffgrad (cons (cons (caaadr e) nil) (append (cdadr e) (cddr e)))
              x))
      ((or (eq fun 'mqapply) (null (setq grad (oldget fun 'grad))))
       (if (not (depends e x)) 0 (idiff%deriv (list e x 1))))
      ((not (= (length (cdr e)) (length (car grad))))
       (merror "Wrong number of arguments for ~:M" fun))
      (t (setq args (idiffmap (cdr e) x))
         (addn (mapcar
            #'mul2
            (cdr (substitutel
              (cdr e) (car grad)
              (do ((l1 (cdr grad) (cdr l1))
                   (args args (cdr args)) (l2))
                  ((null l1) (cons '(mlist) (nreverse l2)))
                (setq l2 (cons (cond ((equal (car args) 0) 0)
                         (t (car l1)))
                       l2)))))
            args)
           t)))))

(defmfun $idiff (&rest args)
  (let (derivlist)
    (ideriv args)))

(defmfun idiff (e x)
  (cond
         (($constantp e) 0.)
	     ((alike1 e x) 1.)
	     ((or (atom e) (member 'array (cdar e) :test #'eq))
;;	      (ichainrule e x))
;;        (idiff%deriv (list e x 1)))
          0)
	     ((kindp (caar e) '$constant) 0.)                    ;New line added
	     ((eq (caar e) 'mrat) (ratdx e x))
	     ((eq (caar e) 'mplus)
	      (simplus (cons '(mplus) (idiffmap (cdr e) x))
		       1.
		       t))
	     ((eq (caar e) 'mequal)
	      (list (car e) ($idiff (cadr e) x) ($idiff (caddr e) x)))
	     ((eq (caar e) '$matrix)
	      (cons (car e)
		    (mapcar 
		     (function (lambda (y) 
				       (cons (car y)
					     (idiffmap (cdr y) x))))
		     (cdr e))))
	     ((eq (caar e) 'mtimes)
 	      (addn (idifftimes (cdr e) x) t))
	     ((eq (caar e) 'mexpt) (idiffexpt1 e x))
	((rpobj e) (diffrpobj e x))
    ((and (boundp '$imetric) (eq (caar e) '%determinant)
      (eq (cadr e) $imetric))
      ((lambda (dummy)
       (setq dummy ($idummy))
       (cond ((eq dummy x) (setq dummy ($idummy))))
       (list '(mtimes simp) 2. e
       (list '($ichr2 simp) (cons smlist (list dummy x))
       (cons smlist (ncons dummy)))))
       nil))
	     ((eq (caar e) 'mnctimes)
	      (simplus (list '(mplus)
			     (list '(mnctimes)
				   ($idiff (cadr e) x)
				   (caddr e))
			     (list '(mnctimes)
				   (cadr e)
				   ($idiff (caddr e) x)))
		       1.
		       nil))
	     ((eq (caar e) 'mncexpt) (idiffncexpt e x))
	     ((eq (caar e) '%integrate) (idiffint e x))
	     ((eq (caar e) '%derivative)
	      (cond ((or (atom (cadr e))
			 (member 'array (cdaadr e) :test #'eq))
;;		     (ichainrule e x))
;;           (idiff%deriv (list e x 1)))
             0)
;;		    ((freel (cdr e) x) 0.)
		    (t (idiff%deriv (list e x 1.)))))
	     ((member (caar e) '(%sum %product) :test #'eq) (idiffsumprod e x))
	     (t (idiffgrad e x))
  )
)

(defun diffrpobj (e x)                  ;Derivative of an indexed object
  (cond
    (               ; Special case: functions declared with coord()
      (and
        (member (caar e) $coord :test #'eq) (null (cdadr e))
        (equal (length (cdaddr e)) 1) (null (cdddr e))
      )
      (delta (ncons x) (cdaddr e))
    )
    (t              ; Everything else
      (nconc
        (list (car e) (cadr e) (caddr e))
        (cond
          (
            (null (cdddr e))
            (ncons x)
          )
          (         ; Derivative indices do not commute when frames are used
            (or $iframe_flag $itorsion_flag)
            (append (cdddr e) (ncons x))
          )
          (t
            (itensor-sort (append (cdddr e) (ncons x)))
          )
        )
      )
    )
  )
)


(defmfun $lc0 (l1) 
       (prog (a b c sign) 
	     (setq a (cdr l1))
	     (ifnot (and a (cdr a)) (return (list '(%levi_civita) l1)))
	     (setq b a)
	loop1(ifnot (fixnump (car a)) (return (list '(%levi_civita) l1)))
	     (and (setq a (cdr a)) (go loop1))
	loop3(setq a (car b) b (cdr b) c b)
	loop2(cond ((= (car c) a) (return 0.))
		   ((< (car c) a) (setq sign (not sign))))
	     (and (setq c (cdr c)) (go loop2))
	     (and (cdr b) (go loop3))
	     (return (cond (sign -1.) (t 1.))))) 
(defmfun $levi_civita (l1 &optional (l2 nil))
	(cond
		((eq l2 nil) ($lc0 l1))
		((like l1 '((mlist)))
		(prog (l) (setq l nil)
		  (do ((i ($length l2) (1- i))) ((< i 1)) (setq l (cons i l)))
		  (return (list '($kdelta simp) (cons smlist l) l2))
		 ))
		((like l2 '((mlist)))
		(prog (l) (setq l nil)
		  (do ((i ($length l1) (1- i))) ((< i 1)) (setq l (cons i l)))
		  (return (list '($kdelta simp) l1 (cons smlist l)))
		))
		(t (merror "Mixed-index Levi-Civita symbols not supported"))
	)
)

;; simplification rules for the totally antisymmetric LC symbol
(defun $lc_l (e)
    (prog (l1 l2 l nn)
	(catch 'match
	  (cond ((atom e) (matcherr)))
	  (cond ((atom (car e)) (matcherr)))
	  (cond ((not (or (eq (caar e) '$levi_civita) (eq (caar e) '%levi_civita))) (matcherr)))
	  (cond ((not ($listp (setq l1 ($covi e)))) (matcherr)))
	  (cond ((not (alike1 '((mlist simp)) (setq l2 ($conti e)))) (matcherr)))
	  (cond ((cdddr e) (matcherr)))
	  (setq nn ($length l1))
	  (setq l nil)
	  (do ((i nn (1- i))) ((< i 1)) (setq l (cons ($idummy) l)))
	  (return (values (list '(mtimes simp) ($kdelta l1 (cons smlist l))
	        (list (cons (caar e) '(simp)) (cons smlist l) (ncons smlist))
	        (list '(mexpt simp) (meval (list 'mfactorial nn)) -1)) t)
	  )
	)
    )
)

(defun $lc_u (e)
    (prog (l1 l2 l nn)
	(catch 'match
	  (cond ((atom e) (matcherr)))
	  (cond ((atom (car e)) (matcherr)))
	  (cond ((not (or (eq (caar e) '$levi_civita) (eq (caar e) '%levi_civita))) (matcherr)))
	  (cond ((not (alike1 '((mlist simp)) (setq l1 ($covi e)))) (matcherr)))
	  (cond ((not ($listp (setq l2 ($conti e)))) (matcherr)))
	  (cond ((cdddr e) (matcherr)))
	  (setq nn ($length l2))
	  (setq l nil)
	  (do ((i nn (1- i))) ((< i 1)) (setq l (cons ($idummy) l)))
	  (return (values (list '(mtimes simp) ($kdelta (cons smlist l) l2)
	        (list (cons (caar e) '(simp)) (ncons smlist) (cons smlist l))
	        (list '(mexpt simp) (meval (list 'mfactorial nn)) -1)) t)
	  )
	)
    )
)

(add2lnc '$lc_l $rules)
(add2lnc '$lc_u $rules)

(declare-top (special e empty $flipflag))

(setq $flipflag nil empty '((mlist simp) ((mlist simp)) ((mlist simp)))) 

(defun nonumber (l)
	(cond
		((numberp (car l)) (nonumber (cdr l)))
		((eq l nil) ())
		(t (cons (car l) (nonumber (cdr l))))
	)
)

(defun removeindex (e l)
 (cond	((null l) nil)
	((atom e)
         (cond ((eq e (car l)) (cdr l))
              (t (cons (car l) (removeindex e (cdr l))))
        ))
	(t (removeindex (cdr e) (removeindex (car e) l)))
 )
)

(defun indices (e)
  (prog (top bottom x y p q r)
    (setq top nil bottom nil)
    (cond
      (
        (rpobj e)
        (setq top (nonumber (conti e))
              bottom (nonumber (append (covi e) (cdddr e))))
      )
      ((atom e))
      (
        (and (eq (caar e) 'mexpt) (eq (caddr e) -1))
        (setq x (indices (cadr e)) bottom (append bottom (car x))
                            top (append top (cadr x)))
      )
      (
        (and (member (caar e) '(%derivative $diff) :test #'eq)
             (or (eq (length e) 3) (eq (cadddr e) 1)))
        (setq x (indices (cadr e)) bottom (append bottom (cadr x))
                            top (append top (car x)))
        (setq x (indices (caddr e)) bottom (append bottom (car x))
                            top (append top (cadr x)))
      )
      (
        (member (caar e) '(mtimes mnctimes mncexpt) :test #'eq)
        (dolist (v (cdr e))
          (setq x (indices v) bottom (append bottom (cadr x))
                              top (append top (car x)))
        )
      )
      (
        (member(caar e) '(mplus mequal) :test #'eq)
        (setq top (indices (cadr e)) bottom (cadr top) top (car top))
        (setq p (intersect top bottom) q (removeindex p bottom)
              p (removeindex p top))
        (dolist (v (cddr e))
          (setq x (indices v) y (cadr x) x (car x))
          (setq r (intersect x y) x (removeindex r x) y (removeindex r y))
          (when
            (not (and (samelists x p) (samelists y q)))
            (merror "Improper indices in ~M" v)
          )
          (setq top (union top r) bottom (union bottom r))
        )
      )
      (
        (member (caar e) '($sum %sum) :test #'eq)
        (setq top (list (caddr e)) bottom (list (caddr e)))
      )
      (
        (member (caar e) '(%idiff $idiff) :test #'eq)
;;; This code would count derivative indices as covariant. However, it is
;;; not needed. If the user wants to count derivative indices, those should
;;; be part of the tensor expression; if the expression is undiff'd, there
;;; must be a reason!
;;        (do
;;          ((f (cddr e) (cddr f)))
;;          ((null f))
;;          (do
;;            ((i 1 (1+ i)))
;;            ((> i (cond ((cadr f) (cadr f)) (t 1))))
;;            (setq bottom (cons (car f) bottom))
;;          )
;;        )
        (setq x (indices (cadr e)) bottom (append bottom (cadr x))
              top (append top (car x)))
      )
    )
    (return (list top bottom))
  )
)

(defmfun $indices (e)
 (prog (top bottom x)
;;	(setq top (indices e) bottom (cadr top) top (car top) x (intersect top bottom))
	(setq top (indices e) bottom (cadr top) top (car top) x (cond ($flipflag (intersect bottom top)) (t (intersect top bottom))))
	(setq top (removeindex x top) bottom (removeindex x bottom))
	(return (cons smlist (list (cons smlist (append top bottom)) (cons smlist x))))
 )
)

(defun samelists (a b)       ;"True" if A and B have the same distinct elements
       (and (= (length a) (length b))
	    (do ((l
		a
		(cdr l)))
		(nil)
		(cond ((null l) (return t))
		      ((member (car l) b :test #'eq))
		      (t (return nil)))))) 

(defmfun $flush n           ;Replaces the given (as arguments to FLUSH) indexed
       (prog (l)          ;objects by zero if they have no derivative indices.
	     (cond ((< n 2) (merror "FLUSH takes at least 2 arguments"))
		   ((not
		      (loop for v in (setq l (listify (f- 1 n)))
			     always (symbolp v)))
;		      (apply 'and (mapcar 'symbolp
;					    (setq l (listify (f- 1 n))) ))
		    (merror "All arguments but the first must be names of
indexed objects")) (t (return (flush (arg 1) l t))))))

(defmfun $flushd n          ;Replaces the given (as arguments to FLUSHD) indexed
       (prog (l)          ;objects by zero if they have any derivative indices.
	     (cond ((< n 2) (merror "FLUSH takes at least 2 arguments"))
		   ((not
		      (loop for v in (setq l (listify (f- 1 n)))
			     always (symbolp v))
;		      (apply 'and (mapcar 'symbolp
;					     (setq l (listify (f- 1 n)))))
		      )
		    (merror "All arguments but the first must be names of
indexed objects")) (t (return (flush (arg 1) l nil))))))

(defun flush (e l flag)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((not (member (caar e) l :test #'eq)) e)
		    ((not (null (cdddr e)))
		     (cond (flag e)
			   (t 0)))
		    (t (cond (flag 0)
			     (t e)))))
	     (t (subst0 (cons (ncons (caar e))
			      (mapcar (function (lambda (q) (flush q l flag)))
				      (cdr e))) e))))

(defmfun $flushnd (e name n)              ;Replaces by zero all indexed objects
       (cond ((atom e) e)               ;that have n or more derivative indices
	     ((rpobj e)
	      (cond ((and (equal (caar e) name)
			  (> (length (cdddr e)) (1- n)))
		     0)
		    (t e)))
	     (t (subst0 (cons (ncons (caar e))
			      (mapcar (function
				       (lambda (q) ($flushnd q name n)))
				      (cdr e))) e))))

(declare-top (special index n dumx))

(defmfun $rename nargs
 (cond ((= nargs 1) (setq index 1)) (t (setq index (arg 2)))) (rename (arg 1)))

(defun rename (e)                           ;Renames dummy indices consistently
       (cond
	((atom e) e)
	((or (rpobj e) (eq (caar e) 'mtimes););If an indexed object or a product
        (and (member (caar e) '(%derivative $diff) :test #'eq) ; or a derivative expression
             (or (eq (length e) 3) (eq (cadddr e) 1)))
    )
	 ((lambda  (l) 
	(simptimes (reorder (cond (l (sublis (itensor-cleanup l (setq n index)) e))(t e))) 1 t))
	  (cdaddr ($indices e))                     ;Gets list of dummy indices
	  ))
	(t            ;Otherwise map $RENAME on each of the subparts e.g. a sum
	 (mysubst0 (simplifya  (cons (ncons (caar e))
				  (mapcar 'rename (cdr e)))
			    t)
		   e))
	))

(defun reorder (e)       ;Reorders contravariant, covariant, derivative indices
       (mysubst0         ;Example: F([A,B],[C,D],E,F)
	(cons
	 '(mtimes)
	 (mapcar
	  #'(lambda (x) 
	    (cond ((rpobj x)
           (setq x ($renorm x))
		   (nconc (list (car x)                              ;($f simp)
				(cons smlist
				      (cond ($allsym (itensor-sort (copy-tree (cdadr x))))
					    (t (cdadr x))))          ;($a $b)
				(cons smlist
				      (cond ($allsym
					     (itensor-sort (copy-tree (cdaddr x))))
					    (t (cdaddr x)))))        ;($c $d)
              (cond ($iframe_flag (cdddr x))
			   (t (itensor-sort (copy-tree (cdddr x)))))))                ;($e $f)
		  (t x)))
	  (cond ((eq (caar e) 'mtimes) (cdr e))
		(t (ncons e)))))
	e))

;;(defun itensor-cleanup (a n)((lambda (dumx)(cleanup1 a)) nil))        ;Sets DUMX to NIL
(defun itensor-cleanup (a nn) (setq n nn dumx nil) (cleanup1 a))
 
(defun cleanup1 (a)
  (and a (setq dumx (implode (nconc (exploden $idummyx)    ;Keep proper order of
				    (exploden n))) n (1+ n))          ;indices
	(cond ((eq dumx (car a)) (cleanup1 (cdr a)))
	      (t (cons (cons (car a) dumx) (cleanup1 (cdr a)))))))
;Make list of dotted pairs indicating substitutions i.e. ((a . #1) (b . #2))

(declare-top (notype n index)(unspecial n dumx index))

(defun itensor-sort (l) (cond ((cdr l) (sort l 'less)) (t l)))
;Sort into ascending order

(defmfun $remcomps (tensor)
       (zl-remprop tensor 'expr) (zl-remprop tensor 'carrays)
       (zl-remprop tensor 'texprs) (zl-remprop tensor 'indexed)
       (zl-remprop tensor 'indexed) (zl-remprop tensor 'tsubr)
       (and (functionp tensor) (fmakunbound tensor))
       '$done)

(defmfun $indexed_tensor (tensor)
  (let (fp new)
    (and (zl-get tensor 'expr) 
	 (merror "~M has expr" tensor))
;    (args tensor  nil)
    (and (setq fp (zl-get tensor 'subr))
	 (progn (setq new (gensym))(putprop new fp 'subr)
		(zl-remprop tensor 'subr)(putprop tensor new 'tsubr)))
    (putprop tensor t 'indexed)
    (putprop tensor (subst tensor 'g '(lambda nn (tensoreval (quote g)(listify nn)))) 'expr)
		(eval (subst tensor 'g (quote (defmfun g nn (tensoreval 'g (listify nn))))))
    '$done))


(defun allfixed (l) 
       (and l (fixnump (car l)) (or (null (cdr l)) (allfixed (cdr l))))) 

(defun tensoreval (tensor indxs)
  ((lambda (der con)
    (and (cdr indxs) (setq con (cdadr indxs) der (cddr indxs)))
  (setq tensor (select tensor (cdar indxs) con der))
  ) nil nil))

(defmfun $components (tensor comp)
  ((lambda (len1 len2 len3 name prop)
    (cond ((not (rpobj tensor)) (merror "Improper 1st arg to COMPONENTS: ~M" tensor)))
    (setq len1 (length (covi tensor)) len2 (length (conti tensor)) len3 (length (deri tensor)))
    (and (not (atom comp))
         (eq (caar comp) '$matrix)
         (cond ((= (f+ (f+ len1 len2) len3) 2)
                (setq name (gensym))
                (set name comp)
                (setq comp name)
               )
               (t (merror "Needs two indices for COMPONENTS from matrix:~%~M" tensor))
         )
    )

    (cond ((and (eq (ml-typep comp) 'symbol) (> (f+ (f+ len1 len2) len3) 0))
           (setq prop 'carrays)
          )
          ((samelists (setq name (append (covi tensor) (conti tensor) (deri tensor))) (cdadr ($indices comp)))
           (setq prop 'texprs comp (cons comp name))
          )
          (t (merror "Args to COMPONENTS do not have the same free indices"))
    )
    (setq tensor (caar tensor) len1 (list len1 len2 len3))
    (cond ((and (setq name (zl-get tensor prop))
                (setq len2 (assoc len1 name :test #'equal))
           )
           (rplacd len2 comp)
          )
          (t (putprop tensor (cons (cons len1 comp) name) prop))
    )
    (or (zl-get tensor 'indexed) ($indexed_tensor tensor))
    '$done
   )
   nil nil nil nil nil
  )
)

(defun select (tensor l1 l2 l3)
  (prog
    nil
    (setq l2 (append (minusi l1) l2) l1 (plusi l1))
    (return
      (
        (lambda
          (prop subs idx)
          (cond
            (
              (and
                (allfixed subs)
                (setq prop (zl-get tensor 'carrays))
                (setq prop (assoc idx prop :test #'equal))
              )
              (cond
                (
                  (alike1
                    (setq prop (cons (list (cdr prop) 'array) subs))
                    (setq subs (meval prop))
                  )
                  0
                )
                (t subs)
              )
            )
            (
              (setq prop (assoc idx (zl-get tensor 'texprs) :test #'equal))
              (sublis
                (mapcar #'cons(cddr prop) subs)
                ($rename (cadr prop) (cond ((boundp 'n) n) (t 1)))
              )
            )
            (
              (setq prop (zl-get tensor 'tsubr))
              (apply
                prop
                (list (cons smlist l1) (cons smlist l2) (cons smlist l3))
              )
            )
            (
              (not (eq l3 nil))
              (apply '$idiff (select tensor l1 l2 (cdr l3)) (list (car l3)))
            )
            (
              t
              (append
                (list (list tensor 'simp) (cons smlist l1) (cons smlist l2))
                l3
              )
            )
          )
        )
        nil (append l1 l2 l3) (list (length l1)(length l2)(length l3))
      )
    )
  )
)


(defmfun $entertensor nargs
  (prog (fun contr cov deriv)
    (cond
      (
        (> nargs 1)
	    (merror "ENTERTENSOR takes 0 or 1 arguments only")
      )
	  (
        (= nargs 0)
	    (mtell "Enter tensor name: ") 
	    (setq fun (meval (retrieve nil nil)))
      )
	  ((setq fun (arg 1)))
    )
    (mtell "Enter a list of the covariant indices: ")
    (setq cov (checkindex (meval (retrieve nil nil)) fun))
    (cond ((atom cov) (setq cov (cons smlist (ncons cov)))))
    (mtell "Enter a list of the contravariant indices: ")
    (setq contr (checkindex (meval (retrieve nil nil)) fun))
    (cond ((atom contr) (setq contr (cons smlist (ncons contr)))))
    (mtell "Enter a list of the derivative indices: ")
    (setq deriv (checkindex (meval (retrieve nil nil)) fun))
    (setq deriv
      (cond ((atom deriv) (ncons deriv))
		    (t (cdr deriv))
      )
    )
    (cond
      (
        (memberl (cdr cov) deriv)
	    (mtell "Warning - there are indices that are both covariant ~
                and derivative%")
      )
    )
    (return ($ishow (nconc (list (list fun 'simp) cov contr) deriv)))
  )
)

(defun checkindex (e f)
  (cond ((and (atom e) (not (eq e f))) e)
	((and (eq (caar e) 'mlist)
	      (loop for v in (cdr e) always (atom v))
;	      (apply 'and (mapcar 'atom (cdr e)))
	      (not (member f e :test #'eq))) e)
	(t (merror "Indices must be atoms different from the tensor name"))))

(defun memberl (a b)
  (do ((l a (cdr l))
       (carl))
      ((null l) nil)
    (setq carl (car l))
    (cond ((and (eq (ml-typep carl) 'symbol)
		(member carl b :test #'equal)) (return t)))))

(defun consmlist (l) (cons smlist l))			;Converts from Lisp list to Macsyma list

;$INDICES2 is similar to $INDICES except that here dummy indices are picked off
;as they first occur in going from left to right through the product or indexed
;object. Also, $INDICES2 works only on the top level of a product and will
;miss indices for products of sums (which is used to advantage by $IC_CONVERT).

(defmfun $indices2 (e)
  (cond ((atom e) empty)
	((not (or (member (caar e) '(mtimes mnctimes) :test #'eq) (rpobj e)))
	 ($indices e))
	(t ((lambda (indices)
	      (do ((ind indices) (free) (dummy) (index))
		  ((null ind)
		   (consmlist (list (consmlist (nreverse free))
				    (consmlist (nreverse dummy)))))
		(setq index (car ind))
		(cond ((member index dummy :test #'equal)
		       (merror "~M has improper indices"
			       (ishow e)))
		      ((member index (cdr ind) :test #'equal)
		       (setq dummy (cons index dummy)
			     ind (delete index (copy-tree (cdr ind))
					 :count 1 :test #'equal)))
		      (t (setq free (cons index free)
			       ind (cdr ind))))))
	    (do ((e (cond ((member (caar e) '(mtimes mnctimes) :test #'eq) (cdr e))
			  (t (ncons e))) (cdr e))
		 (a) (l))
		((null e) l)
	      (setq a (car e))
	      (and (rpobj a) (setq l (append l (covi a) (conti a)
					     (cdddr a)))))))))

(defmfun $changename (a b e)				;Change the name of the indexed object A to B in E
  (prog (old indspec ncov ncontr)			;INDSPEC is INDex SPECification flag
    (cond ((not (or (and (eq (ml-typep a) 'symbol) (setq old a))
		    (and ($listp a) (equal (length (cdr a)) 3)
			 (eq (ml-typep (setq old (cadr a))) 'symbol)
			 (eq (ml-typep (setq ncov (caddr a))) 'fixnum)
			 (eq (ml-typep (setq ncontr (cadddr a))) 'fixnum)
			 (setq indspec t))))
	   (merror "Improper first argument to CHANGENAME: ~M" a))
	  ((not (eq (ml-typep b) 'symbol))
	   (merror "Second argument to CHANGENAME must be a symbol"))
	  (t (return (changename old indspec ncov ncontr b e))))))

(defun changename (a indspec ncov ncontr b e)
  (cond ((or (atom e) (eq (caar e) 'rat)) e)
	((rpobj e)
	 (cond ((and (eq (caar e) a)
		     (cond (indspec (and (equal (length (cdadr e)) ncov)
					 (equal (length (cdaddr e))
						ncontr)))
			   (t t)))
		(cons (cons b (cdar e)) (cdr e)))
	       (t e)))
	(t (mysubst0 (cons (car e)
			   (mapcar (function
				    (lambda (q)
				      (changename a indspec ncov
						  ncontr b q)))
				   (cdr e))) e))))

(defmfun $coord n
  (do ((l (listify n) (cdr l)) (a))
      ((null l) '$done)
    (setq a (car l))
    (cond ((not (eq (ml-typep a) 'symbol))
	   (merror "~M is not a valid name." a))
	  (t (add2lnc a $coord)))))

(defmfun $remcoord (&rest args)
  (cond ((and (= (length args) 1)
	      (eq (car args) '$all))
	 (setq $coord '((mlist)))
	 '$done)
	(t (dolist (c args '$done)
	     (setq $coord (delete c $coord :test #'eq))))))


;; Additions on 5/19/2004 -- VTT

(defmfun $listoftens (e)
  (itensor-sort (cons smlist (listoftens e))))

(defun listoftens (e)
  (cond ((atom e) nil)
	((rpobj e) (list e))
	(t (let (l)
	     (mapcar #'(lambda (x) (setq l (union l (listoftens x) :test #'equal))) (cdr e))
	     l))))

(defun numlist (&optional (n 1))
  (loop for i from n upto $dim collect i))

;;showcomps(tensor):=block([i1,i2,ind:indices(tensor)[1]],
;;	if length(ind)=0 then ishow(ev(tensor))
;;	else if length(ind)=1 then ishow(makelist(ev(tensor,ind[1]=i1),i1,1,dim))
;;	else if length(ind)=2 then ishow(tensor=apply('matrix,makelist(makelist(ev(tensor,[ind[1]=i1,ind[2]=i2]),i1,1,dim),i2,1,dim)))
;;	else for i1 thru dim do (showcomps(subst(i1,last(ind),tensor)),if length(ind)=3 and i1<dim then linenum:linenum+1)
;;);
(defmfun $showcomps (e)
 (prog (ind)
  (setq ind (cdadr ($indices e)))
  (cond ((> 1 (length ind)) ($ishow (meval (list '($ev) e))))
	((> 2 (length ind)) ($ishow (cons smlist (mapcar (lambda (i) (meval (list '($ev) e (list '(mequal) (car ind) i)))) (numlist)))))
	((> 3 (length ind)) ($ishow (list '(mequal) e (cons '($matrix simp) (mapcar (lambda (j) (cons smlist (mapcar (lambda (i) (meval (list '($ev) e (list '(mequal) (car ind) i) (list '(mequal) (cadr ind) j)))) (numlist)))) (numlist))))))
	(t (mapcar (lambda (i)  ($showcomps ($substitute i (car (last ind)) e)) (and (> 4 (length ind)) (< i $dim) (setq $linenum (1+ $linenum)))) (numlist)))
  )
 )
)

; Implementation of the Hodge star operator. Based on the following
; MAXIMA-language implementation:
;
; hodge(e):=
; (
;     [
;         len:length(indices(e)[1]),
;         idx1:makelist(idummy(),i,len+1,dim),
;         idx2:makelist(idummy(),i,len+1,dim)
;     ],
;     funmake("*",makelist(funmake(imetric,[[idx1[i],idx2[i]]]),i,1,dim-len))*
;                 funmake(levi_civita,[[],append(idx1,indices(e)[1])])*e/len!
; )$

(defmfun $hodge (e)
  (prog (len idx1 idx2)
    (setq
      len ($length (cadr ($indices e)))
      idx1 (do ((i $dim (1- i)) l) ((eq i len) l) (setq l (cons ($idummy) l)))
      idx2 (do ((i $dim (1- i)) l) ((eq i len) l) (setq l (cons ($idummy) l)))
    )
    (return
      (append
        (list
          '(mtimes)
          e
          (list '(rat) 1 (factorial len))
          (list
            '($levi_civita)
            '((mlist simp))
            (cons '(mlist simp) (append (reverse idx1) (cdadr ($indices e))))
          )
        )
        (do
          (l)
          ((not idx1) l)
          (setq l (cons (list (list $imetric)
                              (cons '(mlist) (list (car idx1) (car idx2)))) l)
                idx1 (cdr idx1)
                idx2 (cdr idx2)
          )
        )
      )
    )
  )
)

; This version of remsym remains silent when an attempt is made to remove
; non-existent symmetries. Used by $idim below.

(defun remsym (name ncov ncontr)
  (declare (special $symmetries))
  (let ((tensor (implode (nconc (exploden name) (ncons 45)
                                 (exploden ncov) (ncons 45)
                                 (exploden ncontr)))))
    (when (member tensor (cdr $symmetries) :test #'equal)
      (setq $symmetries (delete tensor $symmetries :test #'equal))
      (zl-remprop tensor '$sym)
      (zl-remprop tensor '$anti)
      (zl-remprop tensor '$cyc))))

; This function sets the metric dimensions and Levi-Civita symmetries.

(defmfun $idim (n)
  (remsym '%levi_civita $dim 0)
  (remsym '%levi_civita 0 $dim)
  (remsym '$levi_civita $dim 0)
  (remsym '$levi_civita 0 $dim)
  (setq $dim n)
  (remsym '%levi_civita $dim 0)
  (remsym '%levi_civita 0 $dim)
  (remsym '$levi_civita $dim 0)
  (remsym '$levi_civita 0 $dim)
  ($decsym '%levi_civita n 0 '((mlist) (($anti) $all)) '((mlist)))
  ($decsym '%levi_civita 0 n '((mlist)) '((mlist) (($anti) $all)))
  ($decsym '$levi_civita n 0 '((mlist) (($anti) $all)) '((mlist)))
  ($decsym '$levi_civita 0 n '((mlist)) '((mlist) (($anti) $all)))
)

(defun i-$dependencies (l &aux res)
  (dolist (z l)
    (cond
      ((atom z)
       (merror
         (intl:gettext
           "depends: argument must be a non-atomic expression; found ~M") z))
      ((or (eq (caar z) 'mqapply)
           (member 'array (cdar z) :test #'eq))
       (merror
         (intl:gettext
           "depends: argument cannot be a subscripted expression; found ~M") z))
      (t
       (do ((zz z (cdr zz))
            (y nil))
           ((null zz)
            (mputprop (caar z) (setq y (reverse y)) 'depends)
            (setq res (push (cons (ncons (caar z)) y) res))
            (unless (cdr $dependencies)
              (setq $dependencies '((mlist simp))))
            (add2lnc (cons (cons (caar z) nil) y) $dependencies))
         (cond 
               ((and (cadr zz)
                     (not (member (cadr zz) y)))
                (setq y (push (cadr zz) y))))))))
  (cons '(mlist simp) (reverse res)))

($load '$ex_calc)
($load '$lckdt)
($load '$iframe)
