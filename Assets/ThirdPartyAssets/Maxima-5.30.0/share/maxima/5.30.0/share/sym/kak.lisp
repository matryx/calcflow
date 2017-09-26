; Fichier kak.lsp

;       ***************************************************************
;       *                    MODULE SYM                               *
;       *       MANIPULATIONS DE FONCTIONS SYMETRIQUES                *
;       *        (version01: Commonlisp pour Maxima)                 *
;       *                                                             *
;       *                ----------------------                       *
;       *                  Annick VALIBOUZE                           *
;       *                    GDR  MEDICIS                              *
;       *  (Mathe'matiques Effectives, De'veloppements Informatiques, *
;       *           Calculs et Ingenierie, Syste`mes)                 *
;       *             LITP (Equipe Calcul Formel)                     *
;       *                 Universite' Paris 6,                        *
;       *        4 place Jussieu, 75252 Paris cedex 05.               *
;       *              e-mail : avb@sysal.ibp.fr                      *
;       ***************************************************************

;=============================================================================
;;;              SUR UN PROBLEME DE PIERRE CARTIER
;       Produit k a k ou sommes k a k des racines d'un polynome
; Nous traitons ici le cas des resolvantes symetriques lorsque la fonction
; resolvante est une somme ou un produit. 
;========================================================================
;                      INTERFACE

(in-package :maxima)
(macsyma-module kak)

(mdefprop $somrac
    ((lambda ()) ((mlist) $listei $k)
     ((mprog) (($operation)) (($somrac_init) $listei $k)))
    mexpr)
(add2lnc '(($somrac) $listei $k) $functions)

(mdefprop $prodrac
    ((lambda ()) ((mlist) $listei $k)
     ((mprog) (($operation)) (($prodkak) $listei $k)))
    mexpr)
(add2lnc '(($prodrac) $listei $k) $functions)


; ON RAMENE LES FORME MONOMIALES DE MEME LONGUEUR AVEC LE VRAI
; ORDRE DES LONGUEURS DECROISSANT 
;**************************************************************************
;               DECLARATIONS AU COMPILATEUR
(progn
  (defvar listpi)
  (defvar listei)
  (defvar k)
  (defvar coe)
  (defvar $somrac))
;$somrac_init
;   SOMME KAK EN PASSANT PAR LES FONCTIONS PUISSANCES
;    $p_rac
;Tail merging being done: ($som_Pipj (|1-| i) ..........)
; Tail merging being done: ($Pi2mon pui (|1-| var) ....)
;  SOMME KAK EN PASSANT PAR LES ELEMENTAIRES
;     $e_rac
; CREATION DES PARTION DE LONGEUR ET DE POIDS DONNES
; PRODUIT KAK
;    $prodkak
; CALCUL D'UN COEFFICIENT BINOMIAL
; ECRIVAIN
;** FTOC. WARNING:
;             Franz Lisp declaration 'localf' is currently untranslated
(progn)
;**********************************************************************
;                 SOMME K A K DES RACINES D'UN POLYNOME P

(defun $somrac_init (listei k)
  (cond
    ((equal '$puissances $somrac) ($p_rac listei k))
    (t ($e_rac listei k))))

;           DECOMPOSITION DES FORMES MONOMIALES CONSTITUANT
; CHAQUE FONCTION PUISSANCE DU NOUVEL ALPHABET EN FONCTION DES FONCTIONS
;                  PUISSANCE DES RACINES DE P
;compatible avec (macsyma) et p_sym23 21 et 22.
;consideration des constantes
;de l=1 a bin(n,k) :
;    sl(anciennes racines)=fct(sigmai(anciennes racines); i=1 a n)
;    avec changement de base
;    Sl(nouvelles racines)=Fct(si(anciennes racines))
;    SIGMAl(nouvelles racines)=FCT( Sl(nouvelles racines))
;  plus rapide que som en temps mais pas en espace
; listei=((mlist) e1 ... en)

(defun $p_rac (listei k)
  (setq listei (cdr listei))
  (let ((n (list-length listei)))
    (cond
      ((< n k)
       " impossible ")
      (t
       (meval (list '($bidon2) ))
       (let* ((binnk (binomial n k))
	      (listpi (cdr (meval (list '($ele2pui) binnk 
					(cons '(mlist)(cons n listei))))))
	      (listpi (cons binnk ($som_pipj n binnk nil))))
	 ;; je n'ai pas besoin de faire meval ici puisque le fichier
	 ;; est forcement charge'
	 (pui2polynome '$y listpi))))))
;               (listei  (cdr (pui2ele binnk listpi '$girard))))
 ;         ($fin (1- binnk)
  ;              -1 (list '(mexpt) '$y binnk) listei)))))
;__________________________________________________________________________
;        recherche des Fonctions puissances Pi en fonction des pj
(defun $som_pipj (n i nxlistpi)
  (if (eql 0 i)  nxlistpi
      ($som_pipj n
          (1- i)
          (cons ($p_reduit ($init_pi2mon n i (min k i)))
                nxlistpi))))
;__________________________________________________________________________
;Recherche de l'expressiond'un Pi dans la base des formes monomiales sur S[A]
;*** depart
(defun $init_pi2mon (n i infki)
  (if (eql i infki)
      ($pi2mon n i infki nil
               (binomial (- n i)
                     (- k i)))
      ($pi2mon n i infki nil 1)))
;*** recherche des fonctions monomiales en fonction de leur longueur, var,
;On les range au fur et a mesure dans sym 
(defun $pi2mon (n pui var sym coe)
  (if (eql 0 var) sym
      ($pi2mon n pui
               (1- var)
               (nconc sym ($init_monlgfix pui var (cons nil nil)))
               (div (* coe (- n (1- var))) (- k (1- var))))))

;*** recherche des formes monomiales de S[A], representant Pi (i=pui)
;           et ayant leur longueur egale a var.
(defun $init_monlgfix (pui var slvarh)
  ($monlgfix pui
      (1- var)
      (1- var)
      slvarh (cons 1 nil) (maxote pui var))
  (mapl #'(lambda (ppart)
           (rplaca ppart
                   (cons var
                         (cons ($mult_sym coe (caar ppart))
                               (cdar ppart)))))
        (cdr slvarh)))
;*************************************************************************
;           PAR LES FONCTIONS SYMETRIQUES ELEMENTAIRES ei
;                  DU POLYNOME DE DEPART
;compatible avec e_sym26.l (macsyma)
; consideration des constantes
;de l=1 a bin(n,k) :
;    sl(anciennes racines)=fct(sigmai(anciennes racines); i=1 a n)
;    avec changement de base
;    Sl(nouvelles racines)=Fct(si(anciennes racines))
;    SIGMAl(nouvelles racines)=FCT( Sl(nouvelles racines))
;  plus rapide que som en temps mais pas en espace
(progn)
;listei=(e1,...,en)=((mlist) e1 ... en)
(defun $e_rac (listei k)
  (let ((n (1- (list-length listei))))
    (if (< n k)
        " impossible "
        (if (meval (list '($bidon))); permet de charger le fichier elem ou pas.
        (let* ((binnk (binomial n k)) 
               (listei (cons n (cdr listei))))
              (pui2polynome '$y (cons binnk ($rac2 binnk nil n))))))))

;__________________________________________________________________________
;             Recherche des Pi (i=1 a binnk) en fonction ds ej (j=1 a n)
(defun $rac2 (l nxlistpi n)
  (if (eql 0 l) nxlistpi
      ($rac2 (1- l)
             (cons ($init_piej l (min k l) n) nxlistpi )
             n)))
;__________________________________________________________________________
;      recherche d'un Pi dans la base des ej de S[A]
;*** depart
(defun $init_piej (l infkl n) 
  (if (eql l infkl) 
      ($piej l infkl 0 (binomial (- n l) (- k l)) n )
      ($piej l infkl 0 1 n)))
;*** recherche par les monomiales de S[A], rentrant dans la decomposition
; de Pi et ayant meme longueur. Que l'on decompose en fonction des ej et que
; l'on range longueur apres longueur (longueur = var) dans Pi. 
; i=pui
(defun $piej (pui var $pi coe n) 
  (if (eql 0 var)  $pi
      ($piej pui
             (1- var)
             ($add_sym $pi
                 ($mult_sym coe ($piej_lgfix pui var (cons nil nil) n)))
              (div (* coe (- n (1- var))) (- k (1- var))) 
              n  )))

;*** recherche des formes monomiales a longueur fixe var

(defun $piej_lgfix (pui var slvarh n)
  ($monlgfix pui
      (1- var)
      (1- var)
      slvarh (cons 1 nil) (maxote pui var))
  ($reduit (min pui n) ; varm
           (mapl #'(lambda (ppart)
                    (rplaca ppart (cons var (car ppart))))
                 (cdr slvarh))))
;*********************************************************************
; Calcul des formes monomiales et de leur coefficient intervenant dans Pi
; a longueur fixe et donc aussi a poids i fixe.

; la fonction maxote est commune a : treillis.lsp , resolvante.lsp, kak.lsp
; voir dans util.lsp. Elle donne ici
; le maximun que l'on peut retirer pour avoir p dans E(l,var)(croissance)



;recherche proprement dite (non recursive terminale)
(defun $monlgfix (pui rvar ote slvar poule maxote)
  (cond
    ((> 0 rvar)
     (rplacd slvar (list (cons (car poule) (reverse (cdr poule))))))
    (t
     ($monlgfix ote
		(1- rvar)
		(max (1- rvar)
		     (- (* 2 ote)
			pui))
		slvar ($met pui ote poule) (maxote ote rvar))
     (and (< ote maxote)
	  ($monlgfix pui rvar
		     (1+ ote)
		     (last slvar) poule maxote)))))

(defun $met (pui ote poule)
  (let ((nxcoe ($mult_sym (car poule) (binomial pui ote)))
        (nxpui (- pui ote)))
    (if (eq (caddr poule) nxpui)
        (list* nxcoe
                (1+ (cadr poule))
                (cddr poule))
        (list* nxcoe 1 nxpui (cdr poule)))))
;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
;                    PRODUIT K A K
; listei = ((mlist) e1 .en)
; sans p0
(defun $prodkak (listei k)
  (setq listei (cdr listei))
  (let ((n (list-length listei)))
    (cond
      ((< n k)
       " impossible ") 
      (t
       (meval (list '($bidon2)))
       (let* ((binnk (binomial n k))
	      (listpi 
	       (cdr (meval (list '($ele2pui) (mult binnk k) 
                                 (cons '(mlist) (cons n listei)))))))
	 (pui2polynome '$y
		       (cons binnk ($listpui binnk nil k))))))))

; liste des fonctions puissances dans l'alphabet des racines du polynome
; cherche. 
(defun $listpui (i listpui k)
  (if (eql 0 i) listpui
      ($listpui
          (1- i)
          (cons ($p_reduit (list (list k 1 i k))) listpui)
           k)))
