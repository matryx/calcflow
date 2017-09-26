; partpol.lsp

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
;       CHANGEMENTS DE REPRESENTATIONS SUR k[y1,..., yn][x1,..., xn]
;=========================================================================
; explose
; contract (avec ou sans test)
; partpol   ..................
; part2cont et cont2part
; orbit    
;=============================================================================
;                         INTERFACE

(in-package :maxima)
(macsyma-module partpol)

(mdefprop $tpartpol
    ((lambda ()) ((mlist) $mpol $lvar)
     ((mprog) (($operation)) (($tpartpol_init) $mpol $lvar)))
    mexpr)
(add2lnc '(($tpartpol) $mpol $lvar) $functions)

(mdefprop $p_tpartpol
    ((lambda ()) ((mlist) $mpol $lvar)
     ((mprog) (($operation)) (($p_tpartpol_init) $mpol $lvar)))
    mexpr)
(add2lnc '(($p_tpartpol) $mpol $lvar) $functions)

; Passage d'un polynome symetrique sous la forme rat a ses partitions
(mdefprop $partpol
    ((lambda ()) ((mlist) $pol $lvar)
     ((mprog) (($operation)) (($partpol_init) $pol $lvar)))
    mexpr)
(add2lnc '(($partpol) $pol $lvar) $functions)

; CONTRACTION D'UN POLYNOME SYMETRIQUE

(mdefprop $tcontract
    ((lambda ()) ((mlist) $psym $lvar)
     ((mprog) (($operation)) (($tcontract_init) $psym $lvar)))
   mexpr)
(add2lnc '(($tcontract) $psym $lvar) $functions)

(mdefprop $contract
    ((lambda ()) ((mlist) $psym $lvar)
     ((mprog) (($operation)) (($contract_init) $psym $lvar)))
    mexpr)
(add2lnc '(($contract) $psym $lvar) $functions)

; PASSAGE D'UN POLYNOME SYMETRIQUE CONTRACTE A LA LISTE
; ET INVERSEMENT

(mdefprop $cont2part
    ((lambda ()) ((mlist) $pcont $lvar)
     ((mprog) (($operation)) (($cont2part_init) $pcont $lvar)))
    mexpr)
(add2lnc '(($cont2part) $pcont $lvar) $functions)

(mdefprop $part2cont
    ((lambda ()) ((mlist) $ppart $lvar)
     ((mprog) (($operation)) (($part2cont_init) $ppart $lvar)))
    mexpr)
(add2lnc '(($part2cont) $ppart $lvar) $functions)

(mdefprop $explose
    ((lambda ()) ((mlist) $pc $lvar)
     ((mprog) (($operation)) (($explose_init) $pc $lvar)))
   mexpr)
(add2lnc '(($explose) $pc $lvar) $functions)
;*****************************************************************************




;                   DECLARATIONS AU COMPILATEUR
; $tcontract_init $contract_init
; $p_tpartpol_init p_tpartpol
; $tpartpol_init tpartpol 
; $partpol_init partpol  appellees par apply
; $cont2part_init cont2part
; $partipol partipol 
;** FTOC. WARNING:
;             Franz Lisp declaration 'localf' is currently untranslated
(progn)
;$explose_init
(progn (defvar drapeaux) (defvar modele))
(progn (defvar lvar) (defvar permut))
;*****************************************************************************
;                FORME CONTRACTE D'UN POLYNOME SYMETRIQUE
; appels : contract(pol,lvar) ou tcontract(pol,lvar) t pour test
;     $lvar = [x1, ..., xp] = ((mlist) x1, ..., xp) au depart
;     lvar  = (x1, ..., xp)
;     card = p
;-----------------------------------------------------------------------------
;                     AVEC TEST DE SYMETRIE
; tpartpol ramene des partition sout la forme [part](2). Pour
; l'ecrivain
; de polyn\^ome on peut directement utiliser la fonction 2ecrit.
; Sinon, pour utiliser $distri_ecrit (i.e. ecrit_pol), il faut faire
; ch1repol avant.
;-----------------------------------------------------------------------------
(defun $tcontract_init ($pol $lvar)
  (if (meval (list '($is) (list '(mequal) $pol 0))) 0
      (catch 'rate
        (2ecrit (tpartpol $pol $lvar) (cdr $lvar)))))
;-----------------------------------------------------------------------------
;                    SANS TEST DE SYMETRIE
;-----------------------------------------------------------------------------
(defun $contract_init ($pol $lvar)
  (if (meval (list '($is) (list '(mequal) $pol 0))) 0
      (meval (list
                  '($distri_ecrit)
                  (meval (list '($partpol) $pol $lvar))
                  $lvar))))
;======================================================================
;              PASSAGE D'UN POLYNOME SYMETRIQUE MACSYMA 
;                    A SA REPRESENTATION PARTITIONNEE.
;                
; $partpol_init ramene REP(pol)(1) ............ lexicographique decroissant
; $partpol      ...... REP(pol)(2) ............ lexicographique decroissant
; tpartpol si on desire en plus tester la symetrie
; ----------------------------------------------------------------------------
;                    AVEC TEST DE SYMETRIE
; $pol est un polynome macsyma sous la forme expand
; Si on part d'un polynome non symetrique une erreur est declanchee
; $p_tpartpol_init ramene REP(pol)(1) dans l'ordre des longueurs decroissant
; p_tpartpol       ...... REP(pol)(2) ......................................
;============================================================================
; lpol est sous la forme((coe . [partition](2))...)  mais n'est pas
; un polynome partitionne car on n'a encore retire aucun monome.
(defun $p_tpartpol_init ($pol $lvar)
  (if (meval (list '($is) (list '(mequal) $pol 0))) (cons '(mlist) nil)
      (catch 'rate
        (macsy_list (ch1repol (p_tpartpol $pol $lvar))))))

; appele par pui en drapeau 5 &&&&&&mais si erreur pas de cath !

(defun p_tpartpol ($pol $lvar)
   (if (meval (list '($is) (list '(mequal) $pol 0))) (cons '(mlist) nil)
       (let ((lpol (sort (prep (lect $pol (cons '(mlist) $lvar)))
           '$orlong)))
    (tpartpol2 lpol (list-length (cdr $lvar))))))

(defun $tpartpol_init ($pol $lvar)
   (if (meval (list '($is) (list '(mequal) $pol 0))) (cons '(mlist) nil)
       (catch 'rate
              (macsy_list (ch1repol (tpartpol $pol $lvar))))))

;appele par e_sym (22 ..)  &&&&&&&& pas de catch si erreur
; pol est sous la forme((coe .I)...) les +petit part devant

(defun tpartpol ($pol $lvar)
  (let ((pol (sort (prep (lect $pol $lvar)) '$lexic)))
    (tpartpol2 pol (list-length (cdr $lvar)))))

(defun $lexic (p q)
  (and (not (equal (cdr p) (cdr q))) ($lex (cdr p) (cdr q))))
;--------------------------------------------------------------------------
;                    LE PARTITIONNEUR 
;-----------------------------------------------------------------------------
; Reunir les partitions identiques, rendre le coefficient de la
; forme monomiale associee.
(defun tpartpol2 (lpol card)
  (tpartpol3 card lpol (card_orbit (cdar lpol) card))
  lpol)
;-----------------------------------------------------------------------
;              partitionnement avec test de symetrie : tpartpol3
;                 modification physique sur la forme distribuee
; Tout les monome ont leur exposants represente par [partition](2)
; on a perdu l'information : "exposant attache a une variable"
; dont on n'a pas besoin.
; Un compteur
; Le monome de tete de pol = (coe1 . exposants1)
; le deuxieme monome de tete de pol = (coe2 . exposants2)
; 1- exposants1 est le (p+1)ieme que
;      l'on trouve identique a exposants2 
;          (on a reordonne les exposants)
;     1-1 coe1 distinct de coe2 ==> non symetrique
;     1-2 coe1 = coe2 ==> (compteur ==> compteur-1) : on a un element
;                         supplementaire de l'orbite de exposants2
;                           on l'elimine de pol puisqu'un seul representant
;                          suffit
; 2- exposant1 different de exposants2
;     1-1 compteur > 1 ==> Il manque des elements de l'orbit de exposants2
;     1-2 compteur = 1 ==> - (coe1 . exposants1) represente toute l'orbite
;                            contenu dans pol
;                          - On passe a l'orbite suivante qui
;                              est (coe2 . exposants2) en mettant 
;                              compteur = card_orbite(exposants2)
;--------------------------------------------------------------------------
; pas le cas de la cste
; les coe sont egaux
; ex : 3xy + 3yz + 2xz est non symetrique
(defun tpartpol3 (card pol compteur)
  (if (null (cdr pol))
      (or (eql 1 compteur)
          (throw 'rate '|manque des elements de l'orbite|))
      (let ((coe1 (caar pol)) (coe2 (caadr pol)) (exp1 (cdar pol))
            (exp2 (cdadr pol)))
        (if (equal exp1 exp2)
            (if (equal coe1 coe2)
                (tpartpol3 card (rplacd pol (cddr pol))
                    (1- compteur))
                (throw 'rate
                      '|polynome non symetrique par ses coefficients|))
            (if (eql 1 compteur)
                (tpartpol3 card (cdr pol) (card_orbit exp2 card))
                (throw 'rate '|manque des monomes|))))))
;-----------------------------------------------------------------
;             PARTITIONNEMENT D'UN POLYNOME 
; ENTREES : UN POLYNOME $psym  SOUS FORME DISTRIBUEE OU SINON DONNES PAR UNE
;           REPRESENTATION MACSYMA ET LA LISTE $lvar DE SES VARIABLES
; SORTIE  : LE POLYNOME PARTITIONNE AVEC COMME REPRESENTATION :
;            REP([pol]) = [ppart](1)
;-----------------------------------------------------------------
(defun $partpol_init ($psym $lvar)
 (if (meval (list '($is) (list '(mequal) $psym 0))) (cons '(mlist) nil)
     (cons '(mlist)
        (mapcan #'(lambda ($exposant) 
                          (and (apply '>= (cddr $exposant))
                               (list $exposant)))
                (if (equal 'mlist (caar $psym))
                    (cdr $psym) ; $psym est sous forme distribuee
                    (cdr (meval (list '($distri_lect) 
                                       $psym 
                                       $lvar))))))))

;-----------------------------------------------------------------------
;         FORME CONTRACTEE D'UN POLYNOME ==> REP([pol])(i)
; $cont2part_init --> [$ppart](1) ($ car liste macsyma)
; cont2part et partipol --> [ppart](2)
; appele par $elem_init et $pui_init
;-----------------------------------------------------------------------
; dans lect on met $polcontrac sous forme expand
(defun $cont2part_init ($polcontrac $lvar)
      (if (meval (list '($is) (list '(mequal) $polcontrac 0))) 
          (cons '(mlist)  nil)
          (macsy_list (cont2part_rep1 $polcontrac $lvar))))

(defun cont2part_rep1 (polcontrac $lvar)
  (ordonne_expo (lect polcontrac $lvar)))
; Rend des partitions de type 2

(defun cont2part (polcontrac lvar)
  (ch2repol (cont2part_rep1 polcontrac (cons '(mlist) lvar))))
; on le met sous forme rat
; rat = (((rat simp)...) . listebase . 1)

(defun $partipol (polycontracte)
  (let ((rat ($rat polycontracte)))
    (cons '(mlist) (mapcar 'list2croch (partipol rat)))))

(defun partipol (rat) (partipol1 (cadr rat) (cons nil nil)))

; on aura lpart = ( (coe a1 m1 a2 m2 ....) ....)  par
; accrochage de bord

(defun partipol1 (rat lpart)
  (partipol2 lpart (cdr rat) nil nil)
  (cdr lpart))

; representation des partitions avec un coe et eventuellement des
; exposants nuls en tete.
; rat est la liste elementaire
;on depile ie on remonte
; on a enfin une partition solution
; deplacement en profondeur dans
; le coefficient qui est une liste de base et auquel on enleve la variable.
; on met en instance le deplacement en largeur dans rrat
; on note dans la pile l'etat actuel : la pile la partition
; en construction et le reste des exposant coe a parcourir
; en largeur

(defun partipol2 (lpart rat part pile)
  (if (null rat) (and pile (apply 'partipol2 (cons lpart pile)))
      (if (numberp (cadr rat))
          (partipol2
              (cdr (rplacd lpart
                           (list ($part0 (sort
                                            (cons (car rat) part) '<)
                                         (cadr rat)))))
              (cddr rat) part pile)
          (partipol2 lpart (cdadr rat) (cons (car rat) part)
              (if (cddr rat) (list (cddr rat) part pile) pile)))))
;=======================================================================
(defun $part2cont_init ($ppart $lvar)
  (if (null (cdr $ppart)) 0
       (meval (list '($distri_ecrit) $ppart $lvar))))
;========================================================================
;                 RAMENER TOUT LE POLYNOME SYMETRIQUE
;                   ASSOCIE A UNE FORME CONTRACTEE
;                   dans k[y1, ... ,yn][x1, ... ,xn]
; EN SE SERVANT DE permut ECRIT PAR PHILIPPE ESPERET 
; appel : explose(polynome,[x1, ... ,xp])
;-------------------------------------------------------------------------
;                     L'APPEL PRINCIPAL
; lvar = [x1, ... ,xp] on ne demande pas de preciser y1, ..., yn
; On utilise le lecteur rendant les formes distribuees et
; l'ecrivain qui se trouvent dans le fichier util.l
;-------------------------------------------------------------------------

(defun $explose_init ($pc $lvar)
         (if (meval (list '($is) (list '(mequal) $pc 0))) 0
  (let ((lcoenuplet (lect $pc  $lvar)))
    (meval (list '($distri_ecrit)
                  (cons '(mlist)
                        (mapcan #'(lambda (coenuplet)
                                    (let ((coe (car coenuplet)))
                                           (mapcar #'(lambda (permu) 
                                                        (list* '(mlist)
                                                                coe permu))
                                                   (permut (cdr coenuplet)))))
                                 lcoenuplet))
                   $lvar)))))
;=======================================================================
;                ORBITE D'UN POLYNOME
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;========================================================================
;                       UTILITAIRES LOCAUX
;------------------------------------------------------------------------
; prep : Fonction ordonnant tout d'abord tout les exposants
; dans le sens decroissant puis representent ces exposants
; sous forme [partition](2) (sans les 0)
; Creons tout d'abords une fonction, ordonne_expo, qui a partir d'un polynome
; sous forme distribuee range tout les exposants dans l'ordre
; decroissant

(defun ordonne_expo (pol)
  (mapcar #'(lambda (mon)
             (cons (car mon)
                   (sort (cdr mon) '>)))
          pol))

; Avec changement de repre'sentation
(defun prep (pol) (ch2repol (ordonne_expo pol)))
