;; Fichier arite.lsp

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

;========================================================================= ; 
(in-package :maxima)
(macsyma-module arite)


; cette fonction permet de passer d'une fonction puissance d'une 
; resolvante en p variables 
; a une fonction puissance en n variables gra^ce au the'ore`me de l'arite'
; il faut rajouter un coefficient binomial a chaque partition
; On suppose que les fonctions puissances sont DONNEES SUR LA BASE
; DES FORMES MONOMIALES DE MANIERE PARTITIONNEE DANS LA LISTE $puissances


(mdefprop $arite
    ((lambda ()) ((mlist) $degre $arite $puissances)
     ((mprog) (($operation)) ((arite_init) $degre $arite $puissances)))
    mexpr)
(add2lnc '(($arite) $degre $arite $puissances) $functions)

; Ici le remplacement se re'alise physiquement

(mdefprop $arite_physique
    ((lambda ()) ((mlist) $degre $arite $puissances)
     ((mprog) (($operation)) ((arite_physique_init) $degre $arite $puissances)))
    mexpr)
(add2lnc '(($arite_physique) $degre $arite $puissances) $functions)


(defun arite_init (degre arite $puissances)
    (cons '(mlist)
          (mapcar #'(lambda ($pui) 
                            (cons '(mlist)
                            (n_complete_pui degre arite $pui)))
                  (cdr $puissances))))


(defun arite_physique_init (degre arite $puissances)
    (mapc #'(lambda ($pui) (complete_pui_physique degre arite $pui))
          (cdr $puissances))
     $puissances)

(defun complete_pui_physique (n p $puissance_resolvante)
   (mapc #'(lambda ($part) 
        ; la longueur de la partition n'est pas en tete : (lg coeff . I)
             (let ((lg (longueur (cddr $part))))
                (rplaca  (cdr $part)
                         (* (cadr $part)
                                  (binomial (- n lg)
                                            (- p lg))))))
          (cdr $puissance_resolvante)))
; Ceci seulement pour des coefficients non numeriques :
;                       ($mult_sym (car part)
 ;                                 (binomial (- n lg)
  ;                                          (- p lg))))))
         


; il serait astucieux d'utiliser les rplaca. Pour cela il faut
; garder en memoire bin(n-lg,p-lg), et le retirer de $puif apres
; avoir evalue' en les fonction puissance. Ainsi on perdrait en temps
; mais on gagnerai toute la longueur de $puif en espace.
; attention, il existe une fonction complete_pui dans resolvante

(defun n_complete_pui (n p $puissance_resolvante)
     (mapcar #'(lambda ($part) ; part=(part)(1) comme representation
                       (let ((lg (longueur (cddr $part))))
                            (list* '(mlist)
                                   ($mult_sym (cadr $part)
                                              (binomial (- n lg)
                                                        (- p lg)))
                                   (cddr $part))))
                (cdr $puissance_resolvante)))
