;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

;               COMPUTATIONS OF GROEBNER BASES
;written by D.Lazard (jun. 1988)
;-----------------------------------------------------------
;This package contains (with french comments)
; LIRE : a lisp function which convert a list of multivariate polynomials with
;	integer coefficients from macsyma CRE form to our internal form; the 
;	whole list has to be in CRE form, not only its members.
;
; STAND(CRE_list,ordering) : a macsyma function computing the groebner base of
;	a list of polynomials acceptable by LIRE ; at the present the result 
;	is left in internal form and bounded to the  (macsyma) variables BASE
;	and ERREXP1. ORDERING may be ORDLEX, the pure lexicographical ordering
;	or (better) ORDDEG the degree-reverse-lexicographical ordering; the 
;	ordering on the variables, determined by the ordering in the CRE form,
;	is reversed, when passing from ORDLEX to ORDDEG.
;   Sample usage:
;     (c1)  Stand([x^2-x-u,u^2-1],ordlex);
;
;	Some statistics are given by macsyma global variables:
;		NBSYZ: number of computed syzygies
;		NBRED: number of reduction steps
;		NBREDUC: number of reductions of polynomials already in the 
;			base
;		NBRED0: number of reductions leading to 0
;	Moreover, statistics, and the leading monomials of the polynomials in
;	the base are displayed when a syzygy is computed
;
; (ELIMPOL BASE ORDERING VAR): A lisp function which, from the result of 
;	STAND (called with the same ordering), returns 
;		1 if the input of STAND, viewed as a system of equations
;			has no solution in an algebraic closed field
;		"dimension positive" if the set of solutions in an algebraic
;			closed field is not finite
;		or a macsyma  univariate polynomial in the variable VAR, the 
;			roots of which are the values of the first coordinate
;			of the solutions; the multiplicity of a root is the
;			total multiplicity of the corresponding solutions.
;	This function is mainly useful if ORDERING is ORDDEG.
;
; ELIMPOL_ERR (ORDERING, VAR): a macsyma function which call ELIMPOL on 
;	ERREXP1.
;-----------------------------------

;A FAIRE
;l'ecrivain pour la sortie de grobner
;ne trier que les paires non visitees
;diminuer le nombre de divisions (comme dans le cas lexico ?)

;DECLARATIONS
(declare-top
  (localf
    lire1 lire2
    *ipolmon *iterm isyz *icofpol -ipol -ipol2
    *gpolmon *gterm -gpol *gcofpol
    ired2 idiv-tete2 *gcd primpart
    iredp idivp idivise2
    red1 div-terme divise
    apparier rafraichir reduire inserer vivier
    macplus macmoins macdiv mactimes
    notdivmon densif sousp
    )
  (special
    $nbsyz $nbred $nbreduc $nbred0 ordexp orddeg ordpairs ordlex
    $orddeg $ordlex 
    ))

;; Keep the header for the last cre polynomial formed during a LIRE.
(defvar $header nil)

;(defun $stand ($liste ordre)(grobner (lire $liste ordre) ordre))
(defun $stand ($liste &optional (ordre 'ordlex))
  (let* ((lis (lire $liste ordre))
	 (base  (grobner lis ordre)))
    (cons '(mlist)
	  (loop for w in base collect ($decode_poly w nil)))))




;LE LECTEUR
;Il faut traduire une liste de polynomes CRE; on suppose ici les
;coefficients rationnels
;Il faut avoir appele' RAT sur la liste de polynomes pour toutes les variables
;apparaissent dans les en-tetes de tous les polynomes

;Passer de CRE a polynome en la premiere variable
(defun lire1 (pol var)			;en-tete deja enlevee
	(cond ((numberp pol)  (list (list pol 0)))
	      ((equal (car pol) var)
		 (let ((res (cons nil nil)))
		   (do ((p (cdr pol) (cddr p))
			(r res (cdr r)))
		       ((null p) (cdr res))
		       (rplacd r (cons (list (cadr p) (car p))
				     nil)))))
	      (t (list (list pol 0)))))

;passer de CRE a polynome en n variables
(defun lire2 (pol lvar)
	(cond ((null (cdr lvar))
		(lire1 pol (car lvar)))
	      (t
		(mapcan #'(lambda (u)
			   (mapcar #'(lambda (v)
				      (append v (cdr u)))
			     (lire2 (car u) (cdr lvar))))
		  (lire1 pol (car lvar))))))

;appliquer lire2 a une liste macsyma de polynomes
(defun lire (mliste ordexp)
  (or ($ratp mliste) (setq mliste ($rat mliste)))
  (setq $header (car (second mliste)))
  (or (eq (car $header) 'mrat) (error "bad rat form"))
	(mapcar #'(lambda (u)
		   (sort
		     (lire2 (cadr u)
		            (reverse (cadddr (caadr mliste))))
		     #'(lambda (v w) (ordexp (cdr v) (cdr w)))))
	        (cdr mliste)))

;LES ORDRES
;ordre lexico inverse pour les exposants
(defun orddeg (m1 m2)
	(let ((d (- (apply '+ m1) (apply '+ m2))))
	    (cond ((plusp d) t)
	          ((minusp d) nil)
		  (t (do ((m1 (cdr m1) (cdr m1))
			  (m2 (cdr m2) (cdr m2))
			  (a  (- (car m1) (car m2)) (- (car m1) (car m2))))
		       ((not (and (zerop a) m1)) (minusp a)))))))
(setq orddeg 'orddeg)
(setq $orddeg 'orddeg)

(defun ordlex (m1 m2)
	(do ((m1 (cdr m1) (cdr m1))
	     (m2 (cdr m2) (cdr m2))
	     (a (- (car m1) (car m2)) (- (car m1) (car m2))))
	    ((not (and (zerop a) m1)) (plusp a))))

(setq ordlex 'ordlex)
(setq $ordlex 'ordlex)

(defun ordexp (m1 m2)		;ancienne version
	(let ((d (- (apply '+ m1) (apply '+ m2))))
	    (cond ((plusp d) t)
	          ((minusp d) nil)
		  (t (do ((m1 m1 (cdr m1))
			  (m2 m2 (cdr m2)))
		       ((not (and m1 (= (car m1) (car m2))))
			 (and m1 (< (car m1) (car m2)))))))))

(defun ordpairs (p1 p2)		;nouvelle version moins bonne, mais 
	(let ((d1 (apply '+ (caar p1)))		; ? meilleure pour lexico
	      (d2 (apply '+ (caar p2))))
	   (or (< d1 d2)
	       (and (= d1 d2)
		    (or (funcall ordexp (caadr p2) (caadr p1))
		        (and (equal (caadr p1) (caadr p2))
			     (funcall ordexp (cdadr p2) (cdadr p1))))))))

(defun ordpairs (p1 p2)		;ancienne version
	(let ((exp1 (caar p1))
	      (exp2 (caar p2)))
	   (or (funcall ordexp exp2 exp1)
	       (and (equal exp1 exp2)
		    (or (funcall ordexp (caadr p2) (caadr p1))
		        (and (equal (caadr p1) (caadr p2))
			     (funcall ordexp (cdadr p2) (cdadr p1))))))))

(setq ordpairs 'ordpairs)

;OPERATIONS ELEMENTAIRES PRESERVATIVES (POLYNOMES) COEFFICIENTS GENERAUX

;definition des multiplications
;mactimes peut etre change si les coefficients sont restreints
;par exemple, pour des entiers, on peut prendre "times"

(defun *gpolmon (pol mon)
	(mapcar #'(lambda (u)
		   (*gterm u mon))
	    pol))

(defun *gterm (m1 m2)
	(cons
	  (mactimes (car m1) (car m2))
	  (mapcar '+ (cdr m1) (cdr m2))))


;(defun +pol (p1 p2)			;non destructif
;	(cond ((null p1) p2)		;recursion non terminale
;	      ((null p2) p1)		;non utilise
;	      ((equal (cdar p1) (cdar p2))
;		(let ((a (macplus (caar p1) (caar p2))))
;		  (cond ((zerop a) (+pol (cdr p1) (cdr p2)))
;		        (t (cons
;			     (cons a (cdar p1))
;			     (+pol (cdr p1) (cdr p2)))))))
;	      ((funcall ordexp (cdar p1)(cdar p2))
;		  (cons (car p1) (+pol (cdr p1) p2)))
;	      (t  (cons (car p2) (+pol (cdr p2) p1)))))
;
;(defun syz (pair)
;	(let ((f (caddr pair))
;	      (g (cdddr pair))
;	      (exp (caar pair)))
;	  (-pol (*polmon f (cons (caar g) (mapcar '- exp (cdar f))))
;	        (*polmon (cdr g) (cons (caar f) (mapcar '- exp (cdar g)))))))

;OPERATIONS ELEMENTAIRES DESTRUCTIVES (POLYNOMES) COEFFICIENTS GENERAUX

(defun *gcofpol (c p)	;destructif resultat dans p
	(mapc #'(lambda(u) (rplaca u (mactimes c (car u))))   p))

	;Cette fonction retourne et lie a p1 la difference (cdr p1) - p2
	;iteratif et destructif
	;le polynome nul est retourne (cons nil nil)
(defun -gpol (p1 p2)
	(do ((p p1)
	     (q p2)
	     (e1) (e2))
	    ((null q)
	      (rplaca p1 (cadr p1))
	      (rplacd p1 (cddr p1)))
	    (setq e1 (cdadr p))
	    (setq e2 (cdar q))
	    (cond
	      ((funcall ordexp e1 e2)
		(setq p (cdr p)))
	      ((equal e1 e2)
		(let ((a (macmoins (caadr p) (caar q))))
		  (cond ((zerop a)
			  (rplacd p (cddr p)))
		        (t
			  (setq p (cdr p))
			  (rplaca (car p) a))))
		(setq q (cdr q)))
	      (t
		(setq p (cdr (rplacd p (cons
					  (cons
					    (macmoins 0 (caar q))
					    (cdar q))
					  (cdr p)))))
		(setq q (cdr q))))))

;On definit maintenant la division d'un polynome par une base unitaire
;Le polynome est remplace par le resultat

(defun red1 (pol1 p pol2)		        	;destructif
	(let ((a (mapcar '- (cdar p) (cdar pol2))))	;resultat dans pol1
	  (cond ((minusp (apply 'min a))		;pol2 est unitaire
		  pol1)
	        (t
		  (-gpol p
		        (*gpolmon
			    (cdr pol2)
			    (cons (caar p) a)))))))

(defun div-terme (p1 p  base)
	(do ((a (cdar p) (cdar p))
	     (b nil a))
	    ((eq a b) p)
	    (mapc #'(lambda (u) (red1 p1 p u))       base)))

(defun divise (p1 base)
	(do ((p (cdr (div-terme p1 p1 base)) (cdr (div-terme p1 p base))))
	    ((null p) p1)))

(defun monic (pol)
	(let ((c (caar pol)))
	  (mapc #'(lambda (u)
		   (rplaca u (macdiv (car u) c)))
	    pol)))

;	;Comme ce qui precede, mais pseudo division pour eviter
;	;les divisions de coefficients
;(defun red2 (pol1 pol2)			        	;destructif
;	(let ((a (mapcar '- (cdar pol1) (cdar pol2)))	;resultat dans pol1
;	      (c (caar pol1)))
;	  (cond ((minusp (apply 'min a)) nil)		;sans division
;	        ((-pol (*cofpol (caar pol2) pol1)	;rend nil si pol1..
;		       (*polmon    (cdr pol2)		;..non modifie
;		       	           (cons c  a)))))))
;
;(defun div-tete2 (p1 base)
;	(do ((a (cdar p1) (cdar p1))
;	     (b nil a))
;	    ((eq a b) p1)
;	    (mapc #'(lambda (u) (red2 p1 u))       base)))
;
;(defun divise2 (p1 base)
;	(do ((p (cdr p1) (cdr (div-tete2 p base))))
;	    ((null p) p1)))
;
;OPERATIONS ELEMENTAIRES PRESERVATIVES (POLYNOMES) COEFFICIENTS ENTIERS

;definition des multiplications
;mactimes peut etre change si les coefficients sont restreints
; exemple, pour des entiers, on peut prendre "times"

(defun *ipolmon (pol mon)
	(mapcar #'(lambda (u) (*iterm u mon)) pol))

(defun *iterm (m1 m2)
	(cons
	  (* (car m1) (car m2))
	  (mapcar #'+ (cdr m1) (cdr m2))))

(defun isyz (pair)
	(let ((f (caddr pair))
	      (g (cdddr pair))
	      (exp (caar pair))
	      (a)(b)(d))
	  (setq $nbsyz (1+ $nbsyz)
	        a (caar g)
		b (caar f)
		d (gcd a b)
		a (quotient a d)
		b (quotient b d))
	  (-ipol (*ipolmon f (cons a (mapcar '- exp (cdar f))))
	        (*ipolmon (cdr g) (cons b (mapcar '- exp (cdar g)))))))

;OPERATIONS ELEMENTAIRES DESTRUCTIVES (POLYNOMES) COEFFICIENTS ENTIERS

(defun *icofpol (c p)	;destructif resultat dans p
	(mapc #'(lambda(u) (rplaca u (* c (car u))))   p))

	;Cette fonction retourne et lie a p1 la difference (cdr p1) - p2
	;iteratif et destructif
	;le polynome nul est retourne (cons nil nil)
(defun -ipol (p1 p2)
	(-ipol2 p1 p2)
	(rplaca p1 (cadr p1))
	(rplacd p1 (cddr p1)))

(defun -ipol2 (p1 p2)
	(do ((p p1)
	     (q p2)
	     (e1) (e2))
	    ((null q))
	    (setq e1 (cdadr p))
	    (setq e2 (cdar q))
	    (cond
	      ((funcall ordexp e1 e2)
		(setq p (cdr p)))
	      ((equal e1 e2)
		(let ((a (- (caadr p) (caar q))))
		  (cond ((zerop a)
			  (rplacd p (cddr p)))
		        (t
			  (setq p (cdr p))
			  (rplaca (car p) a))))
		(setq q (cdr q)))
	      (t
		(setq p (cdr (rplacd p (cons
					  (cons
					    (- (caar q))
					    (cdar q))
					  (cdr p)))))
		(setq q (cdr q))))))

	;On definit maintenant la division d'un polynome par une base
	;Le polynome est remplace par le resultat
	;Pseudo division pour eviter
	;les divisions de coefficients
(defun ired2 (pol1 pol2)		        	;destructif
	(let ((a (mapcar '- (cdar pol1) (cdar pol2)))	;resultat dans pol1
	      (c)(b)(d))				;sans division
	  (cond ((minusp (apply 'min a)) nil)		;rend nil si pol1..
	        (t (setq $nbred (1+ $nbred)		;..non modifie
		   	 b (caar pol2)
			 c (caar pol1)
			 d (gcd b c)			;rend (cons nil nil)
			 b (quotient b d)		;si pol1 devient nul
			 c (quotient c d))
		   (*icofpol b (cdr pol1))
		   (-ipol  pol1
		           (*ipolmon  (cdr pol2)
		       	              (cons c  a)))))))

(defun iredp (p1 p q)
	(let ((pp (cdr p))
	      (a) (b) (c) (d))
	  (and pp
	       (cond ((not (minusp (apply 'min
				     (setq a (mapcar '- (cdar pp) (cdar q))))))
		       (setq   	b (caar q)
		    		c (caar pp)
		    		d (gcd b c)
				b (quotient b d)
				c (quotient c d))
		       (*icofpol b p1)
		       (-ipol2 pp
			  (*ipolmon (cdr q)
			            (cons c a)))
		       (rplacd p (cdr pp)))))))

(defun idivp (p1 p base)		;pour idivise2, il faut multiplier
	(do ((a (cdadr p) (cdadr p))	;tout le polynome dividende
	     (b nil a))
	    ((eq a b) (cdr p))
	    (mapc #'(lambda (u) (iredp p1 p u))       base)))

(defun idivise2 (p1 base)
	(do ((p p1 (idivp p1 p base)))
	    ((null p) (primpart p1))))

;OPERATIONS SUR LES COEFFICIENTS

(defun mactimes (a b)
	(meval (list '(mtimes) a b)))

(defun macplus (a b)
	(meval (list '(mplus) a b)))

(defun macmoins (a b)
	(meval (list '(mplus) a (list '(mminus) b))))

(defun macdiv (a b)
	(meval (list '(mquotient) a b)))

;(defun *gcd (lnb)
;	(do ((r (car lnb) (gcd r (car l)))
;	     (l (cdr lnb) (cdr l)))
;	    ((null l) r)))

(defun primpart (p)
	(let ((d (do ((p (cdr p) (cdr p))	;calcul du contenu
		      (g (caar p) (gcd g (caar p))))
		     ((or (eq (abs g) 1) (null p))
		         g) )))
          (or (eq (abs d) 1)
	      (mapc #'(lambda (u)		;diviser par le contenu
	         (rplaca u (quotient (car u) d)))
	        p))
	  p))

;(defun primpart (p)
;	(let ((d (*gcd (mapcar 'car
;			 p))))
;	  (mapc #'(lambda (u)
;		   (rplaca u (quotient (car u) d)))
;	        p)))

;LA BASE STANDARD

;Construction de la base
;Methode a la Buchberger-Bouzeghoub
;base: liste de polynomes
;paires: candidats sygyzies:   ((exp."deja reduite").((exp1.exp2).(f.g)))

;algorithme:
;	reduire tous les polynomes
;	calculer la liste des paires; vivier construit celles qui ne sont
;pas encore visitees
;	reduire la premiere paire, inserer le resultat dans la base et 
;rereduire
;remettre a jour la liste des paires et recommencer

(defun grobner (gener ordexp)
	  (setq $base (cons nil (sort (copy-tree gener)
				#'(lambda (u v) (ordexp (cdar v) (cdar u))))))
	  (let ((paires nil))
	  (setq $nbsyz 0 $nbred 0 $nbreduc 0 $nbred0 0)
	  (reduire $base (cdr $base) (cddr $base))
	  (setq paires
	    (mapcon #'(lambda (u)
		       (let ((exp (cdaar u))
			     (f (car u)))
			 (mapcar #'(lambda (v) (apparier f v exp (cdar v)))
			         (cdr u))))
	            (cdr $base)))
	  (setq paires (cons
			 nil
			 (sort paires ordpairs)))
	  (do ((l (vivier paires))			;parcourir les paires
	       (exp) (exp1) (exp2) (p) (q))
	      ((null l))
	      (setq 	p (car l)     	 exp (caar p)
		    	exp1 (caadr p)	 exp2 (cdadr p))
	      (cond ((cdar p)			;paire deja vue
		      (setq l (cdr l)))
				;on cherche maintenant h dans base tel
				;que h divise exp et que les syzygies
				;(f h) et (g h) ont ete calculees
		    ((do ((l1 (cdr $base) l3)		;"critere 3"
			  (l3 (cddr $base) (cdr l3))
			  (h (cadr $base) (car l3))
			  (exph (cdaadr $base) (cdaar l3)))
		         ((cond ((null l1) t)		;pas trouve de h
			        ((funcall ordexp exph exp) (not (setq l1 nil)))
				((and
				   (not (minusp (apply 'min
						  (mapcar '- exp exph))))
				   (let ((exph1 (mapcar 'max exph exp1)))
				      (or (funcall ordexp exp exph1)
					  (and (equal exp exph1)
					       (funcall ordexp exp2 exph))))
				   (let ((exph2 (mapcar 'max exph exp2)))
				      (or (funcall ordexp exp exph2)
					  (and (equal exp exph2)
					       (funcall ordexp exp1 exph)))))))
		;		(t nil)
			   l1))
		      (setq l (cdr l))
		      (rplacd (car p) t)) 	;le critere 3 est verifie
		    (t
		      (rplacd (car p) t)	;la paire va etre traitee
		      (setq p (isyz p)
			    q (inserer p $base))
		      (cond (q
			      (reduire $base q nil)
			      (format t "~% nbsyz = ~a    nbred = ~a    nbreduc = ~a     nbred0 = ~a    lbase = ~d~%"
				      $nbsyz $nbred $nbreduc $nbred0 (length (cdr base)))

		              (print (escalier (cdr $base)))
			      (print "
")
			      (rafraichir  paires p $base)
			      (setq l (vivier paires)))
			    (t
			      (setq l (cdr l))
			      (format t "~% nbsyz = ~a    nbred = ~a    nbreduc = ~a     nbred0 = ~a    lbase = ~a~% paires a voir = ~d~%"
				      $nbsyz $nbred $nbreduc $nbred0 (length (cdr base)) (length l))
			      
			      )))))	;paire suivante
	  (setq $base (cdr $base))
	  (mapc #'(lambda (u) (idivise2 u $base))
	        $base)				;reduire completement
	  (mapc 'monic $base)))

(defun apparier (p q expp expq)
	  (cons
	    (cons
	      (mapcar #'max expp expq)		;caar
	      (zerop (apply #'+			;cdar ;exposants etrangers
		       (mapcar 'min expp expq))))
	    (cons
	      (cons expp expq)			;caadr	cdadr
	      (cons p q))))			;caddr	cdddr

(defun vivier (paires)
	(let ((v (cons nil nil)))
	  (do ((l (cdr paires) (cdr l))
	       (r v))
	      ((null l))
	      (or  (cdaar l)			;paire deja vue
		   (setq r (cdr (rplacd r
				  (cons (car l) nil))))))
	  (sort (cdr v)  ordpairs)))

(defun rafraichir (paires q base)
	  (let ((nvp (cons nil nil)))
	    (cond ((car q)			;paires p q pour p dans base
		    (do ((l (cdr base) (cdr l))
			 (p (cadr base) (cadr l))
			 (nvpa nvp)		;liste des nouvelles paires
			 (expq (cdar q))
			 (bit t))
		        ((null l))
			(cond ((eq q p)
				(setq bit nil))
			      (bit
				(setq nvpa (cdr (rplacd nvpa
						 (cons (apparier p q
							       (cdar p) expq)
						   nil)))))
			      (t
				(setq nvpa (cdr (rplacd nvpa
						 (cons (apparier q p
							       expq (cdar p))
						   nil)))))))))
	    (do ((l paires)			;rafraichir les anciennes
		 (pp) (f) (g) (ef) (eg))
	        ((null (cdr l)))
		(setq pp (cadr l)
		      f (caddr pp) g (cdddr pp)
		      ef (cdar f) eg (cdar g))
		(cond ((or (null ef)		;enlever si un pol est nul
			   (null eg))
			(rplacd l (cddr l)))
		      ((or (not (equal ef (caadr pp)))	;un pol a change?
			   (not (equal eg (cdadr pp))))
			(setq pp (cond ((funcall ordexp ef eg)
					        (apparier g f eg ef))
				       (t	(apparier f g ef eg))))
			(rplacd l (cons pp (cddr l)))
			(setq l (cdr l)))
		      (t (setq l (cdr l)))))
	    (nconc paires (cdr nvp))))

	  ;recherche des reductibles et reduction des elements de base
(defun reduire (base a b)	;a ---> dernier element reduit
	  (do ((l0 a)			;	(vient d'etre insere)
	       (l1 (cdr a))	;b ---> provenance de cet element
	       (q0 (car a))
	       (q (cadr a) (car l1))
	       (bit (eq a (cdr b)) (or bit (eq l1 (cdr b)))))
	      ((null l1) base)
	      (cond (bit		;reduction par tout ce qui precede
		      (do ((l (cdr base) (cdr l)))	;q reductible?
		          ((cond
			    ((eq l l1) (setq l0 l1))	;q irreductible
			    ((ired2 q (car l))		;ired2 est un predicat
			      (rplacd l0 (cdr l1))	;enlever q
			      (setq $nbreduc (1+ $nbreduc))
			      (and (setq l1 (inserer q base))
				   (setq q0 q b l0 l0 l1
				         bit (eq l0 (cdr b))))
			      t)))))
		    (t			;reduction par q0 seulement
		      (cond ((ired2 q q0)		;q reductible
			      (rplacd l0 (cdr l1))	;enlever q
			      (setq $nbreduc (1+ $nbreduc))
			      (and (setq l1 (inserer q base))
				   (setq q0 q b l0 l0 l1
				         bit (eq l0 (cdr b)))))
			    (t (setq l0 l1)))))
	      (setq l1 (cdr l0))))

(defun inserer (q base)				;retourne le lieu d'insertion
	(do ((l2 base)				;chercher ou`
	     (l3 (cdr base))			;inserer q...
	     (qq (cadr base) (car l3))
	     (expq (cdar q)))
	    ((cond
	       ((null expq)
		 (setq $nbred0 (1+ $nbred0))
		 (setq l2 q))	;si q est nul retourner nil..
	       ((or			;... pour inserer, mais t pour cond
		  (null l3)
		  (funcall ordexp (cdar qq) expq))
		 (rplacd l2 nil)		;pour accelerer la reduction
		 (setq q (primpart q))
		 (setq q (idivise2 q (cdr $base)))	;reduire completement
		 (rplacd l2 (cons  q l3))))	;inserer q
	      (cdr l2))					;lieu a retourner
	    (cond ((ired2 q qq)				;reduire q
		    (setq expq (cdar q))		;et 
		    (setq l2 base))			;repartir
	          (t
		    (setq l2 l3)))
	    (setq l3 (cdr l2))))


;RESOLUTION DE SYSTEMES
;On part d'une base standard pour un ordre compatible avec le degre. Elle
;est representee par une liste de polynomes, chacun d'eux etant une liste de
;monomes de la forme (coeff e1 ... en).

;L'escalier est la liste des vecteurs exposants des monomes dominants.
(defun escalier (base)
  	(mapcar 'cdar base))

;La avriete est vide(i.e. de dim. -1) si l'escalier est reduit au vecteur nul
;(monome constant)
(defun dim-1 (esc)
	(apply 'and (mapcar 'zerop (car esc))) )

;La dimension est 0 si il y a un element de l'escalier sur chaque axe.
;On suppose la base reduite, ce qui implique qu'il n'y a pas 2 elements de 
;l'escalier sur le meme axe
(defun dim0 (esc)
	 (or (dim-1 esc)
	     (do ((l esc (cdr l))
		  (i 0
		     (+ i
		        (- 2 (min 2 
			       (apply '+
				 (mapcar #'(lambda (u) (min u 1))
				         (car l))))))))
	         ((null l)(= i (length (car esc)))))))

;Construction de la liste des monomes sous l'escalier
;Le predicat sousp indique si un monome est sous l'escalier
(defun sous-esc (esc)
	(do ((l ())
	     (m (mapcar #'(lambda (u)u 0) (car esc)))
	     (i (cons 0 nil)))
	    ((null i) l)
	    (rplaca i (1+ (car i)))
	    (cond ((sousp m esc)
		     (setq l (cons (copy-tree m) l))
		     (setq i m))
	          (t
		    (rplaca i 0)
		    (setq i (cdr i))))))

;Test mon2 ne divise pas mon1 pour 2 vecteurs d'exposants
(defun notdivmon (mon1 mon2)
	(minusp (apply 'min (mapcar '- mon1 mon2))))

;sousp est utilise par sous-esc pour tester si un monome est au sous
;l'escalier
(defun sousp (mon esc)
	(apply 'and
	       (mapcar #'(lambda (u) (notdivmon mon u)) esc)))

;ICI, IL FAUT RANGER LE RESULTAT DE LA FONCTION PRECEDENTE POUR L'ORDRE
;COMPATIBLE CHOISI; CE RESULTAT EST UTILISE SOUS LE NOM DE basli

;Construction de la matrice dont le polynome caracteristique a pour racine
;les premieres coordonnees des solutions
;les lignes et colonnes sont indexees par basli, chaque ligne etant en
;representation creuse
;LA FONCTION DIVISE RETOURNE LE RESTE DE LA DIVISION D'UN POLYNOME PAR LA
;BASE STANDARD
(defun umat (basli base)
	(mapcar #'(lambda (u) (divise
			       (cons
				 (cons 1 (rplaca (copy-tree u) (1+ (car u))))
				 nil)
			       base))
	         basli))

;densification d'une ligne
(defun densif (ligne basli)
	(let ((nl (cons '(mlist) nil)))
	  (do ((b basli (cdr b))
	       (l ligne)
	       (c 0 0)
	       (nl nl (cdr (rplacd nl (cons c nil)))))
	      ((null b))
	      (and (equal (car b) (cdar l))
		   (setq c (caar l))
		   (setq l (cdr l))))
	  nl))

;La suite appelle des fonctions macsyma et se trouve dans japsmac.l
;Partie macsyma de japs.l

;matrice macsyma; umatr est le resultat de umat
(defun matmac (basli base)
	(meval (cons '($matrix) (mapcar #'(lambda (u) (densif u basli))
			                 (umat basli base)))))

;Enfin le polynome eliminant, retourne en macsyma
(defun elimpol (base ordexp var1)
	(setq $ratmx t)
	(let ((esc (escalier base))
	      ($ratmx t))
	  (cond
	    ((dim-1 esc) 1)
	    ((dim0 esc)
	      ($charpoly
		   (matmac   (sort (sous-esc esc) ordexp)
		             base)
		   var1))
	    (t "dimension positive"))))

(defun $elimpol_err (ordexp var1)
	(elimpol $errexp1  ordexp var1))

;;;; -*- Mode: LISP; Package: Macsyma; Base:10 -*- Saved by dl
;;;; Macsyma version 309.0
;(mdefprop $elimpol
;          ((lambda nil)
;           ((mlist) $base $var1 $ordre)
;           ((elimpol) $base $var1 $ordre))
;          mexpr)
;(add2lnc '(($elimpol) $base $var1 $ordre) $functions)

(defun decmon (a)
	(mapcar
	    #'(lambda (u v) (list '(mexpt) u v))
	    lvar a))
	    
(defun decterm (a)
	(meval
	   (cons
	      '(mtimes)
	      (cons
	         (car a)
		 (decmon (cdr a))))))

(defun decpol (a)
	(meval 
	   (cons '(mplus)
	      (mapcar 'decterm a))))

(defun decode (lpol lvar)
	(meval 
	   (cons '(mlist)
	      (mapcar 'decpol lpol))))

(defun $decode_poly(poly sample &aux header)
  (cond ((and sample (consp sample)) (setq header (car sample)))
	(t  (setq header $header))) 
  (let ((monoms (loop for v in (fourth header)
		       collect (list v 1 1))))
    (loop for v in poly
	   with ans = 0
	   do (setq ans
		    (pplus
		      ans
		      (loop for deg in (cdr v)
			     for mon in monoms
			     with term = (car v)
			     when (not (eql 0 deg))
			     do (setq term (ptimes term
						(pexpt mon deg)))
			     finally (return term)
			     )))
		    finally (return
			      (cons header
				    (cons ans 1))))))
    
(defun show-lazard (term)
  (cond ((or (atom term) (atom (car term)))(error "bad term"))
        ((consp (caar term)) (loop for v in term do (show-lazard v)))
	(t (displa ($decode_poly term nil)))))
