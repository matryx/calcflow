;; Fichier resolvante.lsp

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
;                   CALCUL DE RESOLVANTES
; ON TRANSFORME UN POLYNOME $pol DE LA VARIABLE $var 
; AVEC UNE FONCTION RESOLVANTE $fonction_resolvante DONT
; LES VARIABLES $list_var. 
; ON NE DOIT PAS METTRE DANS $list_var LES VARIABLES DONT NE DEPEND
; PAS LA FONCTION RESOLVANTE.
;=========================================================================
; REMARQUES D'AMELIORATIONS :
;     1) SI LA TRANSFORMATION EST d'ARITE LE DEGRE DU POLYNOME,
;          ON BALAYE TOUTES LES PARTITIONS POUR RIEN
;     2) IL FAUT AUSSI DISTINGUER LE CAS DE LA TRANSFORMATION A COEFFICIENTS
;        NUMERIQUES ET CELLE A COEFFICIENTS FORMELS
;========================================================================
;                        INTERFACE
(in-package :maxima)
(macsyma-module resolvante)

(mdefprop $linear_resolvante
    ((lambda ()) ((mlist) $pol $var $coeff)
     ((mprog) (($operation)) ((linear_resolvante_init) $pol $var $coeff)))
    mexpr)
(add2lnc '(($linear_resolvante) $pol $var $coeff) $functions)

(mdefprop $resolvante
    ((lambda ()) ((mlist) $pol $var $fonction_resolvante $lvar)
     ((mprog) (($operation)) ((resolvante) $pol $var 
                                                $fonction_resolvante $lvar)))
    mexpr)
(add2lnc '(($resolvante) $pol $var $fonction_resolvante $lvar) $functions)

(mdefprop $somme_orbitale
    ((lambda ()) ((mlist) $coeff poids)
     ((mprog) (($operation)) ((somme_orbitale_init) $coeff poids )))
    mexpr)
(add2lnc '(($somme_orbitale) $coeff poids) $functions)

;=========================================================================

(defun resolvante ($pol $var $fonction_resolvante $list_var)
     (cond  ((equal '$cayley $resolvante)
                 (print " resolvante de Cayley ")
                 ;; (load "resolcayley.lisp")
                 (meval (list '($SUBSTITUTE) 
                           (cons '(mlist)
                                 (mapcar #'(lambda (val $ei)
                                                   (list '(mequal) $ei val))
                                          (cdr (polynome2ele  $pol
							      $var))
                                           '($E1 $E2 $E3 $E4 $E5)))
                                 $RESOLCAYLEY)))
            ((or (equal '$unitaire $resolvante) 
                 (= 2 (list-length $List_var)))
                 (print " resolvante unitaire ")
                 (meval (list 
                              '($RESOLVANTE_UNITAIRE)
                              $pol
                              (meval (list '($EV)
                                             $fonction_resolvante
                                             (meval (list '(MEQUAL)
                                                           (cadr
							    $list_var) 
                                                            $var))))
                              $var)))
           ((equal '$produit $resolvante) ; a`optimiser svp
                 (print " resolvante produit ")
            (meval (list '($prodrac) 
                          (cons '(mlist) (cdr (polynome2ele $pol $var)))
                          (1- (list-length $list_var)))))
           ((equal '$somme $resolvante)
                 (print " resolvante somme ")
            (meval (list '($somrac) 
                          (cons '(mlist) (cdr (polynome2ele $pol $var)))
                          (1- (list-length $list_var)))))
           ((equal '$lineaire $resolvante)
                 (print " resolvante lineaire ")
            (linear_resolvante (mapcar #'(lambda ($var) 
                                            ($coeff $fonction_resolvante 
                                                           $var 1))
                                        (cdr $list_var))
                               (polynome2ele $pol $var)))
           ((equal '$alternee $resolvante)
                 (print " resolvante alternee ")
            (linear_resolvante (mapcar #'(lambda ($var) 
                                            ($coeff $fonction_resolvante 
                                                           $var 1))
                                        (cdr $list_var))
                               (polynome2ele $pol $var)
                               :alternee t))
           ((equal '$symetrique $resolvante) 
                 (print " resolvante symetrique ")
         ; ce n'est pas a resolvante de tester la symetrie
            (symetrique_resolvante $fonction_resolvante 
                                   $list_var 
                                   (polynome2ele $pol $var)))
           ((equal '$groupe $resolvante) 
                 (print " resolvante groupe ") 
                  (print '|non implante|)
                  (groupe_resolvante $fonction_resolvante 
                                      $list_var 
                                      (polynome2ele  $pol $var)))
          

           (t ; ici on peut eventuellement rajouter des tests :
                 ; symetrique ou lineaire
                 (print " resolvante generale ")
            (if (not (consp (cadr $list_var)))
                (meval (list '($direct)
                              (list '(mlist)  $pol)
                               $var
                              $fonction_resolvante
                              (list '(mlist) $list_var)))
                (meval (list '($direct) 
                              $pol 
                              $var 
                              $fonction_resolvante
                              $list_var))))))

; cette fonction semble inutile
(defun cree_subs (val $ei)  (list '(mequal) $ei val))
     

;=========================================================================
;                  RESOLVANTES SYMETRIQUES

; i.e. LA FONCTION RESOLVANTE EST SYMETRIQUE EN p VARIABLES
; COMME L'ORBITE DE LA FONCTION RESOLVANTE SOUS S_p EST LA FONCTION RESOLVANTE,
; LES FONCTIONS PUISSANCES DE CETTE ORBITE SE CALCULENT EN CALCULANT
; LES PUISSANCES DE LA FONCTION RESOLVANTE.
; ENSUITE ON PASSE A S_n (n LE DEGRE DU POLYNOME INITIAL) EN RAJOUTANT
; A CHAQUE PARTITION I LE COEFFICIENT BINOMIAL BIN(n-lg(I),p-lg(I)).

(defun symetrique_resolvante ($fct_resolvante $list_var elementaires)
    (meval (list '($bidon2))) ; charger les fichiers pui.lisp et multmon.lisp
    (meval (list '($multsym) (list '(mlist)) (list '(mlist)) 0))  
    (let* (($pui 3)
           (degre (car elementaires))
           (longueur (list-length (cdr $list_var)) )
             ; recherche de la fonction resolvante sous la forme 
              ; d'un polynome partitionne
           (fct_resolvante  (cont2part_rep1
                                   ($expand (meval (list '($contract) 
                                                   $fct_resolvante 
                                                   $list_var)))
                                   $list_var))
           (degre_resol (binomial degre longueur)))
          (pui2polynome '$y
                      (cons degre_resol
                            (puissances_symetrique_resolvante 
                                   degre_resol 
                                   longueur 
                                   fct_resolvante
                                   fct_resolvante
                                   (meval (list '($ele2pui) 
                                                degre_resol 
                                                (cons '(mlist) elementaires)))
                               )))))


; la fonction multsym realise le produit de deux polynomes symetriques.
; il faut mettre le polynome f plus creux que puif en deuxieme argument
; on pourrait etre tente' de garder un seul polynome puif en memoire
; mais de toute maniere le fait de passer de Rep[1] a Rep[2]
; necessaire a la decomposition en les fonctions puissances imposera
; d'avoir deux polynomes en memoire. Domage! Sinon on gardait les
; anciens coefficients en memoire.
; je n'ai pas cherche' a faire de la recursivite' terminale.

; ici difference avec macsyma dont le commonlisp compile' ne supporte pas
; les print emboite's dans une fonction.

(defun puissances_symetrique_resolvante (ind arite_f f puif $puissances)
     (and (< 0 ind)
         (print (- (- (list-length $puissances) 1) ind))
         (cons  (p_red1 $puissances 
                       (complete_pui (cadr $puissances)
                                      arite_f
                                      puif ))
               (puissances_symetrique_resolvante 
                            (1- ind)
                             arite_f
                             f
                             (and (< 1 ind)
                                  (multsym puif f arite_f))
                             $puissances))))


; Pour le cas qui nous interesse, il est indispensable de ne pas faire
; de remplacement physique sur la liste . Alors on redefinie
; pui_complete
; il serait astucieux d'utiliser les rplaca. Pour cela il faut
; garder en memoire bin(n-lg,p-lg), et le retirer de $puif apres
; avoir evalue' en les fonction puissance. Ainsi on perdrait en temps
; mais on gagnerai toute la longueur de $puif en espace.

(defun complete_pui (n p puissance_resolvante)
     (mapcar #'(lambda (part) ; part=(part)(1) comme representation
                       (let ((lg (longueur (cdr part))))
                            (list* lg
                                   ($mult_sym (car part)
                                              (binomial (- n lg)
                                                        (- p lg)))
                                   (ch2rep (cdr part)))))
                puissance_resolvante))
;=========================================================================
;          ALGORITHMES POUR CALCULER DES RESOLVANTES LINEAIRES

; I.E LA FONCTION RESOLVANTE EST UNE FORME LINEAIRE DANS $K[x_1,...,x_n]$
; SES COEFFICIENTS SONT DANS $coeff : 
; ON TRANSFORME UN POLYNOME $pol DE LA VARIABLE $var (DE DEGRE n).
; ATTENTION !!!!
;               ON SUPPOSE DANS UN PREMIER TEMPS QUE 
;     $coeff NE COMPORTE QUE DES VALEURS NON NULLES 

; SINON LE DEGRE DE LA RESOLVANTE, CELUI DE L'ORBITE DE $coeff SOUS $S_n$,
; N'EST PLUS n!/(n-p)! OU p EST LE NOMBRE DE COEFFICIENTS NON NULS DANS $var
; DE PLUS L'ORBITE DE LA FONCTION RESOLVANTE SOUS L'ACTION DE S_n EST PLUS PETITE.
; IL RESTE DONC A TRAITER CE TRAVAIL SUR LES RESOLVANTES LINEAIRES.
;============================================================================


(defun linear_resolvante_init ($pol $var $coeff &key alternee)
    (linear_resolvante (cdr $coeff) 
                       (polynome2ele $pol $var) 
                       :alternee alternee))

; $bidon2 SERT A CHARGER LE FICHIER PUI.LISP SI CE N'EST DEJA FAIT

(defun linear_resolvante (coeff elementaires &key alternee)
    (meval (list '($bidon2))) 
    (let* ((degre (car elementaires))
                     ; on enleve les coefficients nuls
           (coeff (retirezero coeff)) 
                     ;il faut que les coefficients soient non nuls:
           (longueur (list-length coeff)) 
           (permut_coeff ((lambda (p)
                                  (if alternee  (permut_alterne p) 
                                                 p))
                             (permut coeff)))
           (degre_resol (* (binomial degre longueur) 
                          ((lambda (d) (if alternee (* 2 d) d))
                           (list-length permut_coeff)))))
          (pui2polynome '$y
                        (cons degre_resol
                               (puissances_linear_resolvante 
                                      degre_resol 
                                      longueur 
                                      permut_coeff
                                      (meval (list '($ele2pui) 
                                                 degre_resol 
                                                 (cons '(mlist)
						       elementaires)))
                                      alternee)))))
                                                     

; on utilise directement, p_red1, une fonction interne au fichier
;  pui.lisp evitant ainsi un interfacage inutile et couteux.
; Cette fonction realise le meme travail que la fonction pui
; en imposant que les partitions aient leur longueur en tete
; et quelles soient sous la  representation: 
;     [partition](2) (i.e. (... a_i m_i ...) si I=...a_i^m_i...)

; il faut calculer les fonctions puissances generiques en imaginant
; que le degre du polyn\^ome est longueur (l'arite de la fonction 
; de transformation). Puis ensuite on rajoute le 
; coefficient binomial bin(degre-lg(I),longueur-lg(I)) a chaque partition. 
; On peux meme imaginer que ces coefficients apparaissent souvent  
; donc on va les stocker.

(defun puissances_linear_resolvante 
           (poids longueur permut_coeff $puissances alternee)
       (do ((i poids (1- i))
            (sol nil))
           ((= 0 i) sol)
           (setq sol 
                 (cons (if (and alternee (oddp i)) 0
                           ((lambda (pui)
                                 (if alternee ($mult_sym 2 pui) pui))
                             (p_red1 $puissances 
                                     (pui_linear_resolvante 
                                            permut_coeff
                                            i
                                            longueur
                                            (cadr $puissances)))))
                         sol))))


;-----------------------------------------------------------------------
;       recherche de la r-ieme fonction puissance generique
;    sur la base des formes monomiales en n variables (n=degre(pol))
;  ON CHERCHE DONC LES PARTITIONS DE POIDS FIXE ET DE LONGUEUR BORNEE 
; AVEC EN PLUS COMME COEFFICIENTS :

; BIN(n-lg(I),p-lg(I))*MULTINOMIAL(POIDS(I),I)
;                     *SUM(c^I,c \in ORBIT(coeff,S_p))

; DONC REPRISE AVEC CETTE MODIFICATION DE LA FONCTION ltreillis de SYM

; son poids est r, sa longueur ne doit pas depasser le nombre de 
; coefficients non nuls (i.e. l'arite de la fonction de transformation)
; les permutations distinctes de coeff sont dans : permut_coeff

(defun pui_linear_resolvante (permut_coeff poids longueur n)

  (let ((lpart (cons nil nil)))
    (somme_orbitale poids
               (- longueur 1)
               0 
               (maxote poids longueur) 
               nil 
               lpart permut_coeff poids n longueur)
               (print poids)(cdr lpart)))

; -------------------------------------------------------------------------------
; on doit mettre cela dans $mm car on doit le quoter pour $save qui ne
; peut comprendre (cdr lpart)))

   ;   (set (concat '$ppp poids) (cdr lpart)))) je n'arrive pas a recuperer les pppi
; a la sortie car ce sont des variables speciales (ce que je crois) que je ne 
;peux declarer comme telles et de plus cela surcharge la memoire.

; -------------------------------------------------------------------------------
; Remarque : on s'arrangera plus tard pour eviter les parts nulles, mais alors 
; attention a $multinomial, que l'on devra diviser par (p-lg(I))!.

(defun somme_orbitale (poids rlongueur ote maxote partition lpart 
                       permut_coeff poids_init n p)
  (cond ((minusp rlongueur) 
                     ; les partitions obtenues ne sont pas sous la
		     ; forme [partition](2) avec la longueur en tete
                     ; de plus on passe d'une fonction p-aire a une
		     ; n-aire => (bin (- n lg) (- p lg))
                     ; sans oublier le coefficient multinomial de la partition
         (let* ((partition (sanszero partition))
                (lg (list-length partition))
                (orbit_mon (ev_forme_monomiale permut_coeff 
                                               partition)))
               (if (not (equal 0 orbit_mon))
                (rplacd lpart
                        (list (list* lg ; passer de $S_p$ a $S_n$
; Pour des coefficients de la transformation numeriques :   ($mult_sym
                                      (times
                                         (* (binomial (- n lg) (- p lg)) 
                                            ($multinomial poids_init
                                                   (cons '(mlist) 
                                                 (reverse partition))))
                                          orbit_mon)
                                     (ch2rep (reverse partition))))))))
        (t (somme_orbitale   ote
                         (- rlongueur 1)
                         (max 0 (- (* 2 ote) poids))
                         (maxote ote rlongueur)
                         (cons (- poids ote) partition)
                         lpart
                         permut_coeff poids_init n p)
            (and (< ote maxote)
                   (somme_orbitale poids
                         rlongueur 
                         (1+ ote)
                         maxote
                         partition
                         (last lpart)
                         permut_coeff poids_init n p)))))

; la fonction maxote est commune a : treillis.lsp , resolvante.lsp, kak.lsp
; voir dans util.lsp

;  A PARTIR DES PERMUTATIONS D'UN p-UPLET
;  CONSTRUIRE L'ALPHABET DES MONOMES CONSTITUE' AVEC LES PERMUTATIONS 
; COMME VARIABLES ET LA PARTITION part COMME EXPOSANT. ON CALCULE ICI 
; LA SOMME DES ELEMENT DE CET ALPHABET.
; C'est meme beaucoup mieux ! Cela s'inscrit dans la formule generale 
; des resolvantes lineaires. 
; J'IMPOSE QUE LES COEFFICIENTS NULS NE SOIENT PAS DONNE'S.

(defun ev_forme_monomiale (permut_coeff part)
     ($fadd_sym
            (mapcar #'(lambda (coeff)
                              ($fmult_sym
                                     (mapcar #'(lambda (var expo) 
                                                    ($exp_sym var expo))
                                             coeff part)))
                    permut_coeff)))
;; CECI EST LA VERSION POUR UNE FONCTION DE TRANSFORMATION A COEFFICIENTS
;; NUMERIQUES
(defun ev_forme_monomiale (permut_coeff part)
     (eval (cons 'add
                (mapcar #'(lambda (coeff)
                              (eval (cons 'times
                                       (mapcar #'(lambda (var expo) 
                                                    (expt var expo))
                                             coeff part))))
                        permut_coeff))))
(defun sanszero (rpartition)
      (if (= 0 (car rpartition)) 
          (sanszero (cdr rpartition))
          rpartition))

; liste de coefficients formels ou numerique a laquelle
; on desire retirer les zeros.
(defun retirezero (coeff) 
    (and coeff
         (if (equal 0 (car coeff)) (retirezero (cdr coeff))
             (cons (car coeff) (retirezero (cdr coeff))))))
;=========================================================================
;      ALGORITHMES POUR CALCULER DES RESOLVANTES LINEAIRES-ALTERNEE

; 
; NOUS POUVONS OPTIMISER AU CAS OU LA FONCTION RESOLVANTE EST ALTERNEE
; LE PB EST DE CHANGER (PERMUT COEFF ) (LES PERMUTATIONS SOUS LE GROUPE 
; SYMETRIQUE ) PAR (PERMUT-ALTERNEE COEFF) OU L'ON QUOTIENTE PAR 
; <(1 2)(3 4)...(P-1 P)>
; POUR CELA CA NE COUTE PAS EXTREMEMENT CHER DE CALCULER TOUTES LES 
; PERMUTATIONS ET DE SE DEBARASSER DES INDESIRABLES (ENGENDRANT AINSI QUE 
; L'UNE DES FONCTIONS PERMUTEES DE LA FONCTION RESOLVANTE : f_i OU -f_i)

; CETTE METHODE PERMET DE CALCULER UN POLYNOME DE DEGRE MOITIE MOINS QUE CELUI
; DE LA RESOLVANTE LINEAIRE CHERCHEE. MAIS LE POIDS MAXIMUM DES
; PARTITIONS INTERVENANT DANS LE CALCUL (LE DEGRE DE LA RESOLVANTE)
; RESTE LE MEME
;=========================================================================


(defun permut_alterne (permut_coeff) 
    (do ((sol (list (car permut_coeff)))
         (p (cdr permut_coeff) (cdr p))
         (c (cadr permut_coeff) (cadr p))) ; erreur ici corige'e en juin 92!!!
        ((null p)  sol) 
        (and (pas-dans (mapcar #'- c) sol) (setq sol (cons c sol))))); Mars 93

;ENCORE UNE FOIS LES COEFFICIENTS DE LA FONCTIONS DE TRANSFORMATION
; SONT SUREMENT NUMERIQUES : DE PLUS CELA NE MARCHE PAS AVEC RAT :
;        (and (pas-dans (mapcar #'$moins_sym c) sol) (setq sol (cons c sol)))))

;; ATTENTION ICI TOUT DOIT-ETRE NUMERIQUE:
(defun pas-dans (u list) 
  (or  (null list) 
       (and (not (equal u (car list)))
            (pas-dans u (cdr list)))))
       

(defun resol_carre (degre puissances)
   (un-sur-deux (cdr (puireduc_init degre (cons (/ degre 2) puissances)))))

(defun un-sur-deux (liste); liste =(p1,p2,p3,....)  
   (and liste (cons (cadr liste) (un-sur-deux (cddr liste)))))
