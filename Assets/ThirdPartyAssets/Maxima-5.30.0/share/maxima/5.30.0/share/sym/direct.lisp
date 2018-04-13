; Fichier direct.lsp

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

;=============================================================================
;                CALCULS D'IMAGES DIRECTES, D'ORBITES ...
;                    DANS LE CAS LE PLUS GENERAL
;            LA FONCTION resolvante PEUX ETRE AMENEE A UTILISER CE PROGRAMME
;        LORSQUE LA FONCTION RESOLVANTE N'A AUCUNE PROPRIETE EXPLOITABLE
; Si CE N'EST SON ARITE EN COMPARAISON DES DEGRES DES POLYNOMES A TRANSFORMER
;===========================================================================
;                        INTERFACE

(in-package :maxima)
(macsyma-module directnew)

;; Fonctions MACSYMA
(mdefprop $direct
    ((lambda ()) ((mlist) $list_pol $x $fonction $list_list_var)
     ((mprog) (($operation))
      (($direct_init) $list_pol $x $fonction $list_list_var)))
    mexpr)
(add2lnc '(($direct) $list_pol $x $fonction $list_list_var) $functions)
(mdefprop $orbit
    ((lambda ()) ((mlist) $fonction $list_var)
     ((mprog) (($operation)) (($orbit_init) $fonction $list_var)))
    mexpr)
(add2lnc '(($orbit) $fonction $list_var) $functions)
(mdefprop $multi_orbit
    ((lambda ()) ((mlist) $fonction $list_var)
     ((mprog) (($operation)) (($multi_orbit_init) $fonction $list_var)))
    mexpr)
(add2lnc '(($multi_orbit) $fonction $list_var) $functions)
(mdefprop $pui_direct
    ((lambda ()) ((mlist) $multi_orbit $list_list_var $ldegre)
     ((mprog) (($operation))
      (($pui_direct_init) $multi_orbit $list_list_var $ldegre)))
    mexpr)
(add2lnc '(($pui_direct) $multi_orbit $list_list_var $ldegre) $functions)
; SYM pour Macsyma
;============================================================================
;                     IMAGE DIRECTE
; P1,....,Pp polynomes dans k[X^i] et de degre di respectivement
; X^i = (x^i_1, x^i_2, ..., x^i_di) pour i= 1 ... p
; X = (X^1,X^2,...,X^p) 
; P polynome dans k[X]
;                ON CALCUL P_*(P1,...,Pp)
;============================================================================
;                   DECLARATIONS AU COMPILATEUR FRANZLISP
(progn
  (defvar $direct)
  (defvar $pui)
  (defvar $elem)
  (defvar sauvedrapeau))
; $orbit_init
; $multi_orbit_init 
; $pui_direct_init
; $direct_init 
;** FTOC. WARNING:
;             Franz Lisp declaration 'localf' is currently untranslated
(progn)
;============================================================================
;                  Orbite d'un polynome dans k[y1,...,yn] sous S_n
;                      k eventuellemnt anneau de polynomes
;----------------------------------------------------------------------------
; On fait permuter ses variables et on l'applique a chacune
; de ces permutations. Puis on elimine les egaux au fur et 
; a mesure
;----------------------------------------------------------------------------
(defun $orbit_init ($p $lvar) (cons '(mlist) (orbit $p  $lvar)))
(defun orbit ($p $lvar)
  (let ((p_dist (lect $p $lvar))
        (list$pol (list $p)))
    (orbit2 list$pol p_dist (cdr $lvar) nil)
    list$pol))
; les permutations circulaires ne changeraient rien
(defun orbit2 (list$pol p_dist f_lvar d_lvar)
  (and (cdr f_lvar)
       (mapc #'(lambda (f_lvar2)
                (let (($pol (ecrit_pol  p_dist (append d_lvar f_lvar2))))
                  (or (contient list$pol $pol)
                      (flet ((franz.attach (newelt oldlist)
                                 "equivalent to Franz Lisp 'attach'."
                                 (progn
                                   (rplacd oldlist
                                    (cons (car oldlist) (cdr oldlist)))
                                   (rplaca oldlist newelt))))
                        (franz.attach $pol list$pol))))
                (orbit2 list$pol p_dist (cdr f_lvar2)
                        (append d_lvar (list (car f_lvar2)))))
             (permut_circu (cdr f_lvar) (list (car f_lvar))))
       (orbit2 list$pol p_dist (cdr f_lvar)
               (append d_lvar (list (car f_lvar))))))
; on ne ramene pas l'identite
(defun permut_circu (debut fin)
  (cond
    ((null (cdr debut)) (list (cons (car debut) fin)))
    (t (cons (append debut fin)
             (permut_circu (cdr debut) (cons (car debut) fin))))))
(defun contient (list$pol $pol)
    (catch 'trouve
        (progn
          (mapc #'(lambda ($pol2)
                   (and (meval (list '($is)
                                     (list '(mequal) $pol $pol2)))
                               (throw 'trouve t)))
                list$pol)
          nil)))
;==========================================================================
;        CALCUL DE L'ORBITE DU POLYNOME P SOUS S_d1xS_d2x...xS_dp
;--------------------------------------------------------------------------
(defun $multi_orbit_init ($p $llvar)
  (cons '(mlist) (multi_orbit_init $p (cdr $llvar))))
; sous S_0
(defun multi_orbit_init ($p llvar)
  (cond
    ((null llvar) (list $p))
    (t (multi_orbit (orbit $p (car llvar)) nil (cdr llvar)))))
; On se deplace en largeur dans l'arbre ie. on fait agir tout S_i avant
; de passer a S_(i+1).
; En d'autres termes : On calcul l'orbite du polynome P sous
; S_1 x ... x S_i et on en deduit son orbite sous S_1 x ... x S_(i+1).
; Quand on passe a S_(i+1) si un des polynomes generes par l'action de
; S_(i+1) (sur un polynome q de l'etape S_i ) est egal a un 
; polynome r (distinct de q bien entendu!) genere par
; l'action de S_i on elimine froidement r. (Pourquoi refaire ce qui vient
;  d'etre fait ?)
; au depart i = 1  et  llvar = (X^2 X^3 ... X^p) (cf. probleme general)
; on a toute l'orbite sous S_1 x ... x S_(i+1).
; si i+1 =p 
; passe a i+2
; on ote de lpoli les polynomes communs a orbit
(defun multi_orbit (lpoli lpoli+1 llvar)
  (cond
    ((null lpoli)
     (cond
       ((null (cdr llvar)) lpoli+1)
       (t (multi_orbit lpoli+1 nil (cdr llvar)))))
    (t (let ((orbit (orbit (car lpoli) (car llvar))))
         (epure lpoli (cons nil (copy-tree orbit)))
         (multi_orbit (cdr lpoli) (nconc orbit lpoli+1) llvar)))))
; Que fait epure? He bien il enleve physiquement de (cdr l1) tout
; les polynomes se trouvant eventuellement dans (cdr l2) en les diminuant
; toutes deux physiquement.

(defun epure (l1 l2)
  (and (cdr l1)
       (cond
            ((catch 'trouve 
                          (dans l2 (cadr l1)))
                             ; car on calcul la difference
                             ; on l'a retire de l2 (ne reviendra pas)
          (epure (rplacd l1 (cddr l1)) l2)) ; allez! oust!
                  ; l2 diminue' physiquement egalement
         (t (epure (cdr l1) l2)))))

; on regarde si l'oppose de $-pol est dans l2
; si oui on le retire de l2 et on repond : t sinon : nil

(defun dans (l2 $pol)
  (and (cdr l2)
       (cond
         ((meval (list '($is) (list '(mequal) (cadr l2) $pol)))
          (rplacd l2 (cddr l2))
             ; on en profite pour le retirer de l2
            (throw 'trouve t))
         (t (dans (cdr l2) $pol)))))

;=========================================================================
;        REMARQUE SUR CE QUI PRECEDE
;=========================================================================
; On peut se demander : Pourquoi ne pas lire une seule fois
; le polynome en le mettant sous la forme d'une
; liste (c m1 m2 ... mp) representant cm1m2...mp ? ou c est
; un element de k et chaque mi un monome de k[X^i].
; Ceci n'a pas ete fait pour 3 raisons
; la premiere etant que la donnee d'entree (le polynome P) est
; forcement petite (sinon les calculs satureront par la suite)
; et que le calcul de sa multi_orbite est negligeable devant
; ce qui l'attend. Alors au vu des difficultes mises en evidence
; par les deux autres raisons on se dit que ce n'est vraiment 
; pas la peine.
; la seconde est qu'on est amene a comparer l'egalite des polynomes
; a chaque etape i de multi_orbit. Et que meme si les monomes
; de k[X^1,...,X^(i-1)] sont mis en coefficients comment fait-on
; pour ceux dependant des X^q (q > i)?
; La troisieme est que le coefficient lie a un monome en X^i est
; en fait un polynome en les autres groupe de variables et qu'il
; faudra bien les reunir d'une facon ou d'une autre.
; Apres maintes considerations j'ai opte pour la version decrite
; precedemment qui oblige a repasser le lecteur sur le polynome et ses
; permute's a chaque fois que l'on veut calculer son orbite sous S_di.
;===========================================================================
;                CALCUL DES FONCTIONS PUISSANCES
;          SOUS FORME CONTRACTE SOUS S_d1 x ... x S_dp = S_D
; SOIT O = {f1,f2, ...,fo} des polynomes en X^1, en X^2, ... et
; en X^p. On cherche les fonctions puissances P_r(O) (r= 1..o)
; sur O mais dans une forme contracte sous
; S_D (O etant bien constitue pour que cela soit possible).
;(ie. ne prendre qu'un monome par orbite)
;-----------------------------------------------------------------------------
;                       EXEMPLE
; P = a*x + b*y 
;  X^1=(x,y) elementaires : e1, e2, puissances : p1, p2
;  X^2=(a,b) elementaires : f1, f2, puissances : g1, g2.
; O = (ax + by , ay + bx)
; P_1(O) = ax + by + ay + bx = (a + b)(x +y)
;         forme contracte : ax
;         P_1(O) = e1*f1 = p1*g1
; P_2(O) = (ax + by)^2 + (ay + bx)^2
;        = a^2x^2 + b^2y^2 + a^2y^2 + b^2y^2 + 2(axby + aybx)
;        = (a^2 + b^y^2)(x^2 + y^2) + 4axby
;         forme contracte : a^2x^2 + 4axby
;         P_1(O) = (e1^2 - 2e2)(f1^2 - 2f2) + 4e2f2
;                = p2g2 + (p1^2 - p2)(g1^2 -g2)
;-----------------------------------------------------------------------------
;                    CONTRAINTE
; SE DEBARASSER SYSTEMATIQUEMENT DE TOUT MONOMES SI ON EN A DEJA 
; UN DANS SA MULTI_ORBITE AFIN D'EVITER AU MIEUX L'EXPLOSION EN ESPACE.
; CE QUI EXPLIQUE EN PARTIE POURQUOI ON PREFERE LES FONCTIONS PUISSANCES
; AUX FONCTIONS SYMETRIQUES ELEMENTAIRES SUR O.
; On ne garde que les monomes representes par des multipartitions.
; Remarque : il serait plus efficace d'utiliser le logiciel de
; Jean-Charles Faugere.
;-----------------------------------------------------------------------------
; 1_ l'appel et la boucle principale
; on retire le degre en tete
(defun $pui_direct_init ($or $llvar $ldegre)
  (cons '(mlist)
        (cdr (pui_direct (cdr $or) 
                         (mapcar 'cdr (cdr $llvar))
                         (cdr $ldegre)))))

(defun pui_direct (or llvar ldegre) 
  (let* (
        (ldegre_arite (mapcar 'list 
                             ldegre
                             (mapcar 'list-length llvar)))
        (degre_resol (* (list-length or) ;le degre de P_*(P1,...,Pp)
                        (apply '* (mapcar #'(lambda (nb) (apply
							'binomial nb))
                                           ldegre_arite)))))
    (do ((o (and (print degre_resol) (1- degre_resol))
            (and (print o) (1- o)))
         (listpui (list  (pui_contract 0 or llvar degre_resol ldegre_arite))
                  (cons  (pui_contract 0 or llvar o ldegre_arite) listpui)))
        ((eql 0 o) (cons degre_resol listpui)))))

; 2_ Obtention de la rieme fonction puissance 
; dans Or on a des polynomes macsyma
; dans $pui_contract des polynomes sous formes contractees
; on ne conserve que les monomes dont les exposants correspondent a des
; multipartitions
; Ramene un polynome macsyma
;-----------------------------------------------------------------------
(defun pui_contract ($pui_cont or llvar r ldegre_arite)
  (cond
    ((null or) $pui_cont)
    (t (pui_contract
           ($add_sym (multi_partitions ($exp_sym (car or) r) 
                                       llvar
                                       ldegre_arite)
               $pui_cont)
           (cdr or) llvar r ldegre_arite))))

; on jette les momones a exposants non multi_partitionne dans $pol.
; map applique a toute les sous-listes et rend son deuxieme arguments
; ie. la premiere liste.

(defun multi_partitions ($pol llvar ldegre_arite)
  (do ((rllvar (cdr (reverse llvar)) (cdr rllvar))
       (rldegre_arite (cdr (reverse ldegre_arite)) (cdr rldegre_arite))
       (pol_multipartitionne
           (garde_que_partition_init (cons nil
                                           (lect $pol
                                                (cons '(mlist) (car (last
						        	     llvar)))))
                                     (car (last ldegre_arite)))))
      ((null rllvar)
       (if pol_multipartitionne
           (multi_ecrit pol_multipartitionne llvar) 0))
    (setq pol_multipartitionne
          (apply 'nconc
                 (mapl #'(lambda (p_m)
                          (rplaca p_m
                                  (distribu (cdar p_m)
                                      (garde_que_partition
                                        (cons nil (lect (caar p_m) 
                                                        (cons '(mlist)
							    (car rllvar))))  
                                         (car rldegre_arite)))))
                       pol_multipartitionne)))))

; le coefficient binomial permet de tenir compte de l'arite' 
; de la fonction resolvante.

(defun garde_que_partition_init ($pol_dist degre_arite)
  (do ((pol $pol_dist) (degre (car degre_arite)) (arite (cadr degre_arite)))
      ((null (cdr pol)) (cdr  $pol_dist))
    (let ((exposants (cdadr pol)))
         (cond
              ((apply #'>= exposants) 
               (setq pol (cdr pol))
               (let ((lg (longueur exposants)))
                     (rplaca pol (list ($mult_sym (caar pol)
                                              (binomial (- degre lg) 
                                                        (- arite lg)))
                                       exposants))))
      (t (rplacd pol (cddr pol)))))))

; remarque en common lisp : (>= 4 3 2 1 0) ==> true permet de tester
; si une liste est une partition

(defun garde_que_partition ($pol_dist degre_arite)
  (do ((pol $pol_dist)
       (degre (car degre_arite)) 
       (arite (cadr degre_arite)))
      ((null (cdr pol)) (cdr $pol_dist))
      (let ((exposants (cdadr pol)))
           (cond
                ((apply '>= exposants) 
                 (setq pol (cdr pol))
                 (let ((lg (longueur exposants)))
                      (rplaca (car pol)
                              ($mult_sym (caar pol)
                                         (binomial (- degre lg) 
                                                   (- arite lg))))))
               (t (rplacd pol (cddr pol)))))))

(defun distribu (multipartition pol_part)
  (mapcar #'(lambda (coe_partition)
             (cons (car coe_partition)
                   (cons (cdr coe_partition) multipartition)))
          pol_part))
;=========================================================================
;                BOUCLE PINCIPALE DE L'IMAGE DIRECTE
;=========================================================================
(defun $direct_init ($lpol $x $p $llvar)
  (direct (cdr $lpol) $x $p  $llvar))

(defun direct (l$pol $x $p $llvar)
  (cond ((equal '$parallele $directnew) 
         (direct-parallele l$pol $x $p $llvar))
        (t 
           (let* (
                  ($multi_base (if (equal '$elementaire $direct)
                                   (macsy_list (multi_polynome2ele l$pol $x))
                                   (multi_ele2pui (multi_polynome2ele l$pol $x))))
                  (pui_* (pui_direct  (multi_orbit_init $p (cdr $llvar))
			              (mapcar 'cdr (cdr $llvar))
                                      (mapcar 'cadr (cdr $multi_base) )))
                  (degre (car pui_*))) 
                 (pui2polynome '$y
                               (cons degre
                                     (mapcar #'(lambda ($pi)
                                                   (multi_decomp_init
                                                             $pi
                                                             $multi_base
                                                             $llvar ))
                                             (cdr pui_*))))))))

; Ici on calcule les fonctions puissances des racines de la resolvante
; au fur et a mesure. Avant nous calculions les fonctions puissances
; des racines de la resolvante generique sur la base des formes
; monomiales et ensuite on specialisait. En fait nous n'exploitions
; pas l'aspect parallele du calcul.

(defun direct-parallele (l$pol $x $p $llvar)
(let* (
       (llvar (mapcar 'cdr (cdr $llvar)))
         ($multi_base (if (equal '$elementaire $direct)
                         (macsy_list (multi_polynome2ele l$pol $x))
                         (multi_ele2pui (multi_polynome2ele l$pol $x))))
       (multi_orbite (multi_orbit_init $p (cdr $llvar)))
       (ldegre (mapcar 'cadr (cdr $multi_base) )) ; degres des polynomes
       (ldegre_arite (mapcar 'list 
                             ldegre
                             (mapcar 'list-length llvar)))
       (degre_resol (* (list-length multi_orbite) ;le degre de P_*(P1,...,Pp)
                        (apply '* (mapcar #'(lambda (nb) (apply
							'binomial nb))
                                           ldegre_arite)))))
           (do ((r (and (print degre_resol) (1- degre_resol))
                   (and (print r) (1- r)))
            (listpui (list (multi_decomp_init (pui_contract 0 
                                                    multi_orbite
                                                    llvar
                                                    degre_resol 
                                                    ldegre_arite)
                                            $multi_base
                                            $llvar ) )
                    (cons  (multi_decomp_init  (pui_contract 0 
                                                    multi_orbite
                                                    llvar
                                                    r
                                                    ldegre_arite)
                                            $multi_base
                                            $llvar)
                             listpui)))
        ((eql 0 r)  
         (pui2polynome '$y  (cons degre_resol listpui))))))

;====================================================================
;  FONCTIONS SYMETRIQUES ELEMENTAIRES DES RACINES DES POLYNOMES DE
; l$pol EN LA VARIABLE $y.

(defun multi_polynome2ele (l$pol $y)
  (mapcar #'(lambda ($pol) (polynome2ele $pol $y)) l$pol))

;=========================================================================
;          DECOMPOSITION D'UN POLYNOME SYMETRIQUE CONTRACTE
;             EN PLUSIEURS PAQUETS DE VARIABLES
;           DONT LES FONCTIONS SYMETRIQUES ELEMENTAIRES
;               RESPECTIVES SONT DONNEES
;=========================================================================

(defun multi_decomp_init ( $multi_pc $multi_base $llvar)
  (cond
    ((equal '$elementaires $direct)
     (meval (list '($multi_elem)  $multi_base
                                  $multi_pc $llvar)))
    (t (meval (list '($multi_pui) $multi_base
                                   $multi_pc $llvar)))))

; on a les fonctions symetriques elementaires des racines des differents
; polynomes. On recupere en fonction d'elles leurs fonctions puissances.

(defun multi_ele2pui (multi_lelem)
  (cons '(mlist) 
       (mapcar #'(lambda (lelem) 
                     (meval (list '($ele2pui)
                                   (car lelem) 
                                   (cons '(mlist) lelem))))
                multi_lelem)))



