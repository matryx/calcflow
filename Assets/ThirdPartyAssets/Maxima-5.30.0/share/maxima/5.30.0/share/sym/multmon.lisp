; Fichier multmon.lsp

;       ***************************************************************
;       *                    MODULE SYM                               *
;       *       MANIPULATIONS DE FONCTIONS SYMETRIQUES                *
;       *        (version01: Commonlisp pour Maxima)                 *
;       *                                                             *
;       *                ----------------------                       *
;       *                  Annick VALIBOUZE                           *
;       *                    GDR MEDICIS                              *
;       *  (Mathe'matiques Effectives, De'veloppements Informatiques, *
;       *           Calculs et Ingenierie, Syste`mes)                 *
;       *             LITP (Equipe Calcul Formel)                     *
;       *                 Universite' Paris 6,                        *
;       *        4 place Jussieu, 75252 Paris cedex 05.               *
;       *              e-mail : avb@sysal.ibp.fr                      *
;       ***************************************************************


;=========================================================================
;               PRODUIT DE DEUX FORMES MONOMIALES

; multsym(ppart1,ppart2,card)

; LE CALCUL N'EST ABSOLUMENT PAS SYMETRIQUE, IL FAUT QUE ppart2 soit
; plus creux que ppart1 si l'on desire etre efficace.
;============================================================================

(in-package :maxima)
(macsyma-module multmon)

(progn (defvar terparts))

(progn)
;----------------------------------------------------------------------------

;*********************************************************************
;                   BOUCLE PRINCIPALE
;*********************************************************************
; faire 2 option
; 1- contractee
; 2- partitionnee de type 1

;-------------------------------------------------------------------------
;                         INTERFACE

(mdefprop $multmon
    ((lambda ()) ((mlist) $mi $mj $card)
     ((mprog) (($operation)) (($multmon_init) $mi $mj $card)))
    mexpr)
(add2lnc '(($multmon) $mi $mj $card) $functions)

(mdefprop $multsym
    ((lambda ()) ((mlist) $2ppart $1ppart $card)
     ((mprog) (($operation)) (($multsym_init) $1ppart $2ppart $card)))
    mexpr)
(add2lnc '(($multsym) $1ppart $2ppart $card) $functions)

;-------------------------------------------------------------------------

(defun $multsym_init ($1ppart $2ppart card)
      (macsy_list (multsym (mapcar 'cdr (cdr $1ppart))
                                  (mapcar 'cdr (cdr $2ppart)) card)))

; Remarque : multmon restitue des ppart ordones
; dans l'ordre lexicographique decroissant.

(defun multsym (1ppart 2ppart card)
  (do ((1ppart 1ppart (cdr 1ppart))
       (solution nil))
      ((null 1ppart) solution) 
      (do ((2ppart 2ppart (cdr 2ppart))
           (term1 (car 1ppart)))
          ((null 2ppart))
          (setq solution (somme_coef (merge 'list
                                             solution
                                             (mult_term term1 
                                                        (car 2ppart)
                                                        card)
                                             #'lex_term))))))
(defun mult_term (term1 term2 card)
     (mapcar #'(lambda (term)
                       (rplaca term
                              ($mult_sym  (car term)
                                          ($mult_sym  (car term1)
                                                      (car term2)))))
             (multmon (cdr term1) (cdr term2) card)))


; Produit de deux formes monomiales sous formes contractees

(defun $multmon_init ($mi $mj card)
  (cond
    ((or (equal 0 $mi) (equal 0 $mj)) 0)
    (t (let ((i (fmon2part $mi)) (j (fmon2part $mj)))
         (ecrit_pol (multmon i j card) (lvar card nil))))))


; PASSAGE D'UNE FORME MONOMIALE A SA REPRESENTATION PARTITIONNELLE
         ; [forme monomiale] ---> [partition](1)

(defun fmon2part (m)
  (let ((rat (cadr ($rat m))))
      (sort (chexpo (caddr rat) (list (cadr rat))) '>)))

(defun chexpo (liste lexpo)
  (cond
    ((numberp liste) lexpo)
    (t (chexpo (caddr liste) (cons (cadr liste) lexpo)))))

;---------------------------------------------------------------------
; Produit de deux formes monomiales sous forme de partition , type 1.
; Rend la plus grande partition en premier.
; Les partitions ont des zeros a la fin

; exemple :
;<1>: (multmon '(2 1 1) '(3 1 1) 5)
;((1 5 2 2 0 0) (2 5 2 1 1 0) (6 5 1 1 1 1) (2 4 2 2 1 0)
; (3 4 2 1 1 1) (1 4 3 2 0 0) (2 4 3 1 1 0) (6 3 3 1 1 1)
; (4 3 2 2 1 1) (2 3 3 2 1 0) (3 3 2 2 2 0))
;---------------------------------------------------------------------
; on complete I avec de zeros

(defun multmon (i j card)
  (let ((i (fini0 i card)) (j (epur0 j)))
    (let ((lperm (permut j)) (terparts (cons nil nil)))
      (mapc #'(lambda (j) (parti_som_init i j terparts)) lperm)
      (cdr (nreverse terparts)))))

; on rajoute les zeros 

(defun fini0 (i card)
  (nconc i
         (make-list (- card (list-length i))
                :initial-element 0)))
; on enleve les zeros

(defun epur0 (j)
  (mapcan #'(lambda (part)
             (and (< 0 part)
                  (list part)))
          j))

;******************************************************************
; OBTENTION DE CERTAINES DES PARTITIONS SOLUTION A PARTIR D'UNE
; PERMUTATION DE J
;******************************************************************

; I=(i1 i2 ... in) avec i1>i2>...>in>=0 n=card
; J est une permutation de Jo (sans les parts nulles)
; on desire faire la somme de I avec toutes les partitions K,
; construites a partir de J par insertion de zeros, et telles
; que IK=((i1,k1),(i2,k2), ... , (in , kn)) soit dans l'ordre
; lexicographique decroissant.
; On associe alors a K+I le quotient des 2 valeurs c1 et c2.
; Ou c1 est le nombre de permutations laissant I+K invariante
; et c2 le nombre de permutations laissant IK invariante.

(defun parti_som_init (i j terparts) (parti_som i j nil nil))

; I se met au fur et a mesure dans RI, et K dans RK.
; les solutions IK se rangent dans terparts.
;conservation de l'ordre lex
; sur les couples solutions
; si le cardinal le permet, on va pouvoir rajouter des zeros

(defun parti_som (i j ri rk)
  (cond 
    ((null j)
     (flet ((franz.attach (newelt oldlist)
	      "equivalent to Franz Lisp 'attach'."
	      (progn
		(rplacd oldlist (cons (car oldlist) (cdr oldlist)))
		(rplaca oldlist newelt))))
       (franz.attach
	(somme_coe (nconc (reverse i) ri)
		   (append (make-list (list-length i) :initial-element 0) rk))
	terparts)))
    (t
     (and (or (not ri)
	      (< (car i) (car ri))
	      (not (< (car rk) (car j))))
	  (parti_som (cdr i) (cdr j) (cons (car i) ri)
		     (cons (car j) rk)))
     (and (not (< (list-length (cdr i)) (list-length j)))
	  (parti_som (cdr i) j (cons (car i) ri) (cons 0 rk))))))


(defun somme_coe (i k)
  (let ((i+k (sort (mapcar '+ i k) '>))
        (cik (mapcar 'cons i k)))
    (cons (coei+k i+k cik) i+k)))

(defun coei+k (i+k cik)
  (/ (card_stab i+k 'eql) (card_stab cik 'equal)))
