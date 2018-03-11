; Fichier chbase.lsp

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
;                     CHANGEMENTS DE BASES :
 
;         PUI2COMP, PUI2ELE, ELE2PUI, ELE2COMP, COMP2PUI, COMP2ELE
;=============================================================================

(in-package :maxima)
(macsyma-module chbase)

(mdefprop $ele2pui
    ((lambda ()) ((mlist) $max $listei)
     ((mprog) (($operation)) (($ele2pui0) $max $listei)))
    mexpr)
(add2lnc '(($ele2pui) $max $listei) $functions)
;                   PASSAGE DES ELEMENTAIRES AUX COMPLETES
(mdefprop $ele2comp
    ((lambda ()) ((mlist) $max $listei)
     ((mprog) (($operation)) (($ele2comp_init) $max $listei)))
    mexpr)
(add2lnc '(($ele2comp) $max $listei) $functions)
;*******  recherche des puissances entre min et max connaissant 
;                    les elementaires et les puissances 
; Recherche des dependance des fonction puissance avec celle d'indice
; inferieur aux cardinal.
(mdefprop $puireduc
    ((lambda ()) ((mlist) $max $listpi)
     ((mprog) (($operation)) (($puireduc_init) $max $listpi)))
    mexpr)
(add2lnc '(($puireduc) $max $listpi) $functions)
;----------------------------------------------------------------------------
;                Passage des puissances aux elementaires
(mdefprop $pui2ele
    ((lambda ()) ((mlist) $max $listpi)
     ((mprog) (($operation)) (($pui2ele0) $max $listpi)))
    mexpr)
(add2lnc '(($pui2ele) $max $listpi) $functions)
;            PASSAGE DES PUISSANCES AUX COMPLETES
(mdefprop $pui2comp
    ((lambda ()) ((mlist) $max $listpi)
     ((mprog) (($operation)) (($pui2comp_init) $max $listpi)))
    mexpr)
(add2lnc '(($pui2comp) $max $listpi) $functions)
;*******  recherche des elementaires entre min et max connaissant 
;                    les puissances et les elementaires 
(mdefprop $elereduc
    ((lambda ()) ((mlist) $min $max $listei $listpi)
     ((mprog) (($operation)) (($elereduc0) $min $max $listei $listpi)))
    mexpr)
;                 A PARTIR DES COMPLETES
;       LES PREMIERES FONCTIONS ELEMENTAIRES
(mdefprop $comp2ele
    ((lambda ()) ((mlist) $max $listhi)
     ((mprog) (($operation)) (($comp2ele_init) $max $listhi)))
    mexpr)
(add2lnc '(($comp2ele) $max $listhi) $functions)
;        OBTENIR LES PREMIERES FONCTIONS PUISSANCES
(mdefprop $comp2pui
    ((lambda ()) ((mlist) $max $listhi)
     ((mprog) (($operation)) (($comp2pui_init) $max $listhi)))
    mexpr)
(add2lnc '(($comp2pui) $max $listhi) $functions)
;==============================================================
;            RECAPITULATIF DES FONCTIONS


; ELEMENTAIRES AUX PUISSANCES
; $ele2pui0  ele2pui
; $puireduc_init puireduc_init
;  $puireduc0 puireduc
; ELEMENTAIRES AUX COMPLETES
; $ele2comp_init ele2comp
; PUISSANCES AUX ELEMENTAIRES
; $pui2ele0 pui2ele
; $elereduc0 elereduc
; PUISSANCES AUX COMPLETES
; $pui2comp_init pui2comp
; COMPLETES AUX ELEMENTAIRES
;$comp2ele_init comp2ele
; COMPLETES AUX PUISSANCES
;$comp2pui_init comp2pui

;                      DECLARATION DES FONCTIONS LOCALES

;** FTOC. WARNING:
;             Franz Lisp declaration 'localf' is currently untranslated

;                            VARIABLES LOCALES
(progn)
(progn
  (defvar lpui)
  (defvar lelem)
  (defvar lcomp)
  (defvar card)
  (defvar en)
  (defvar $pui2ele))
;______________________________________________________________________
;                 CHANGEMENTS DE BASES
;card le cardinal de l'alphabet
;lpui=(card p1 ....)
;lelem=(card e1 ...)    et on a e1=p1=h1
;lcomp = (card h1 ...)
;-------------------------------------------------------------------------
;                        A PARTIR DES ELEMENTAIRES
;-------------------------------------------------------------------------
;                  OBTENIR LES PREMIERES FONCTIONS PUISSANCES
;        -------------- Les fonctions d'appel ----------------
;entree sortie macsyma


(defun $ele2pui0 (max $lelem); on cherche de 0 a max
                             ;$lelem=((mlist) card e1 e2....)
  (cons '(mlist) (ele2pui_init max (cdr $lelem))))

;l'evaluation

; si il n'y a rien a changer avec card numerique
(defun ele2pui (max lelem)
  ($e_calbasepui (car lelem) 2 max (list (cadr lelem) (car lelem))
      lelem))

(defun ele2pui_init (max lelem)
  (let* ((lelem ($debut '$e lelem max)) (l (list-length lelem))
         (card (car lelem)))
    ($e_calbasepui card 2 max (list (cadr lelem) card)
        (if (< l (1+ max))
            (nreverse
                ($complbase '$e (reverse lelem) l
                    (1+ max)))
            lelem))))


(defun $puireduc_init (max $lpui)
  (cons '(mlist) (puireduc_init max (cdr $lpui))))

; lpui=(card p1 .... p(l-1))

(defun puireduc_init (max lpui)
  (let ((card (car lpui)) (l (list-length lpui)))
    (cond
      ((< max l)
       lpui)
      ((or (not (numberp card))
           (not (< card max)))
       (rangpi2 max lpui l))
      ((< card l)
       ($e_calbasepui card l max (reverse lpui)
           (pui2ele card lpui '$girard)))
      (t (let ((lpui (rangpi2 card lpui l)))
           ($e_calbasepui card
               (1+ card)
               max (reverse lpui) (pui2ele card lpui '$girard)))))))

;listpi =(p0,...,pm) et lg = m+1

(defun rangpi2 (n listpi lg)
  (if (< n lg)
      listpi (append listpi (rangpi3 n lg nil))))
(defun rangpi3 (n i flistpi)
  (if (< n i)
      (nreverse flistpi)
      (rangpi3 n
               (1+ i)
               (cons (flet ((franz.concat (&rest args)
                                "equivalent to Franz Lisp 'concat'."
                                (values (intern
                                         (format nil "~{~A~}" args)))))
                       (franz.concat '$p i))
                     flistpi))))

(defun $puireduc0 (min max $lpui $lelem)
  (cons '(mlist) (puireduc min max (cdr $lpui) (cdr $lelem))))

(defun puireduc (min max lpui lelem)
  ($pm_ei2 min max lpui ($debut '$e lelem max)))

(defun $pm_ei2 (min max lpui lelem)
  (let ((card (car lelem)))
    (let ((l1 (list-length lpui)))
      (if (or (eql 0 l1) (eql 1 l1))
          (flet ((franz.nthcdr (ind lis)
                     "equivalent to Franz Lisp 'nthcdr'."
                     (let ((evalind (eval ind)))
                       (if (minusp evalind) (cons nil lis)
                           (nthcdr evalind lis)))))
            (franz.nthcdr min
                (apply '$e_calbasepui
                       (cons card
                             ($pe_rallonge 2 min max
                                 (list (car lelem) (cadr lelem)) lelem)))))
          (flet ((franz.nthcdr (ind lis)
                     "equivalent to Franz Lisp 'nthcdr'."
                     (let ((evalind (eval ind)))
                       (if (minusp evalind) (cons nil lis)
                           (nthcdr evalind lis)))))
            (franz.nthcdr min
                (apply '$e_calbasepui
                       (cons card ($pe_rallonge l1 min max lpui lelem)))))))))

;-------------------   Les calculs -----------------------------------       
; pm = (-1)^{m+1}e_m + somme (-1)^{i+1} e_i p_{m-i} pour i=1 a m-1
; lelem = (e0 e1 ...) rlpui=(e1 e0)
;----------------------------------------------------------------------
;m>=2 les plus grands pm sont devant

(defun $e_calbasepui (card min max rlpui lelem)
  (let ((lelem (chsigne lelem)))
    (do ((rlpui rlpui)
         (m min
            (1+ m)))
        ((< max m))
      (flet ((franz.attach (newelt oldlist)
                 "equivalent to Franz Lisp 'attach'."
                 (progn
                   (rplacd oldlist (cons (car oldlist) (cdr oldlist)))
                   (rplaca oldlist newelt))))
        (franz.attach
            ($e_calpui card rlpui lelem
                (if (< card m)
                    0 ($mult_sym m (nth m lelem))))
            rlpui)))
    (nreverse rlpui)))

; Calcul de la m-ieme fonction puissance

(defun $e_calpui (card rlpui lelem pn)
  (do ((j 1
          (1+ j))
       (base (cdr lelem) (cdr base)) (rbase rlpui (cdr rbase)) (pn pn))
      ((or (< card j)
           (null (cdr rbase)))
       pn)
    (setq pn ($add_sym pn ($mult_sym (car base) (car rbase))))))

;---------------   mise sous bonne forme d'initialisation --------------

; il manque des ei de i=l2 a max
(defun $pe_rallonge (l1 min max list1 list2)
  (let ((l2 (list-length list2)))
    (list (max min l1) max
          (if (< l1 min)
            ; il manque les pi de i=l1 a min-1
              ($complbase '$p (reverse list1) l1 min) (reverse list1))
          (if (or (< l2 max)
                  (eql l2 max))
                 ; il manque des ei de i=l2 a max
              (nreverse
                  ($complbase '$e (reverse list2) l2
                      (1+ max)))
              list2))))
;-------------------------------------------------------------------------------
;               OBTENIR LES PREMIERES FONCTIONS COMPLETES
; p.14 du Macdonald : h_0 = e_0 = 1
; somme des (-1)^r e_r * h_{n-r} = 0 pour tout n >= 1
; lelem = liste des elementaires
; lcomp = liste des completes
;-------------------------------------------------------------------------------
(defun $ele2comp_init (max $lelem)
  (cons '(mlist) (ele2comp_init max (cdr $lelem))))

(defun ele2comp_init (max lelem)
  (let* ((lelem ($debut '$e lelem max)) (l (list-length lelem)))
    (ele2comp max
        (if (< l (1+ max))
            (nreverse
                ($complbase '$e (reverse lelem) l
                            (1+ max)))
            lelem))))

; si il n'y a rien a completer dans lelem

(defun ele2comp (max lelem)
  (e_calbasecomp (car lelem) 2 max (list (cadr lelem) (car lelem))
      lelem))

; on utilise la meme fonction pour le passage des elementaires aux 
; puissances : e_calpui.


(defun e_calbasecomp (card min max rbarrivee badepart)
        ;m>=2 les plus grands pm sont devant
  (let ((badepart (chsigne badepart)))
    (do ((rbarrivee rbarrivee)
         (m min (1+ m)))
            ((< max m))
      (flet ((franz.attach (newelt oldlist)
                 "equivalent to Franz Lisp 'attach'."
                 (progn
                   (rplacd oldlist (cons (car oldlist) (cdr oldlist)))
                   (rplaca oldlist newelt))))
        (franz.attach
            ($e_calpui card rbarrivee badepart
                (if (< card m)
                    0 (nth m badepart)))
            rbarrivee)))
    (nreverse rbarrivee)))

;-------------------------------------------------------------------------
;                 A PARTIR DES FONCTIONS PUISSANCES
;--------------------------------------------------------------------------
;           OBTENIR LES PREMIERES FONCTIONS SYMETRIQUES ELEMENTAIRES
; Si on ne cherche qu'une seule fonction symetrique elementaire
; on utilise une formule close. Cela sera specifie par un drapeau
; pour pui2ele . Il sera avec Girard ou close.
; $lpui = ((mlist) p0 p1 ...)
;-------------------------------------------------------------------------
; on cherche de 0 a max
(defun $pui2ele0 (max $lpui)
  (cond
    ((equal '$girard $pui2ele)
     (cons '(mlist) (pui2ele_init max (cdr $lpui))))
    (t (pui2ele_init max (cdr $lpui)))))

; si il y a a rajouter
(defun pui2ele_init (max lpui)
  (let* ((lpui ($debut '$p lpui max)) (l (list-length lpui)))
    (pui2ele max
             (if (< l (1+ max))
                 (nreverse
                     ($complbase '$p (reverse lpui) l
                         (1+ max)))
                 lpui)
             $pui2ele)))

; si il n'y a rien a rajouter dans la liste des fonctions puissances.
(defun pui2ele (max lpui $pui2ele)
  (cond
    ((equal '$girard $pui2ele) (girard_pui2ele max lpui))
    (t (cond
         ((< (car lpui) max)  0)
         (t (macdonald_pui2ele max (cdr lpui)))))))

;.............. AVEC LA FORMULE CLOSE ..................................

(defun macdonald_pui2ele (n lpui)
  (let ((en 0))
    (macdonald2 n 0 (list (cons n (reverse lpui)) (expt -1 n)))
    en))

(defun macdonald2 (exposant ote poule)
  (cond
                      ;on a une partition de poids n
    ((eql 0 exposant) (setq en ($add_sym en (termine poule))))
    (t (macdonald2 ote
           (max 0
                (- (* 2 ote) exposant))
           (met exposant ote poule))
       (let ((ote (1+ ote)))
         (and (< ote exposant)
              (macdonald2 exposant ote poule))))))

; termine ramene epsilon_I*z_I*p_I avec |I|=n
; remarque : (nth i liste) ramene le i+1 ieme element de la liste.

(defun termine (poule)
  (let* ((aj+1 (cadddr poule)) (mj+1 (caddr poule)) (rlpui (car poule))
         (puiaj+1 (nth (- (car rlpui) aj+1)
                       (cdr rlpui))))
    ($divi_sym ($mult_sym ($exp_sym puiaj+1 mj+1) (car (last poule)))
        (* (cadr poule) (expt aj+1 mj+1) (factorielle mj+1)))))

; met construit au fur et a mesure epsilon_I*z_I et p_I pour |I|
; strictement inferieure a n

; au depart poule = ( (n  pn ... p1) (-1)^n)
; poule = (rlpui epsilon_I*z_I*(-1)^{mj+1} mj+1 aj+1 p_I)
; ou I = (a1 m1 ... aj mj) = [partition](2) avec n >= a1
; et rlpui = (aj paj pa(j-1) ... p2 p1)



(defun met (exposant ote poule)
  (cond
    ((null (cddr poule))
     (list (car poule)
           (* (cadr poule) -1)
           1
           (- exposant ote)
           1))
    (t (let ((ak (- exposant ote)); nouvelle part obtenue
             (aj+1 (nth 3 poule)); part courante, multiplicite en cours
                                 ; de calcul
             (rlpui (car poule))
             (coe (* -1 (cadr poule)))) ;on change la signature a chaque
                                             ; nouvelle part obtenue
     ;puisque la longueur augmente de 1.
         (cond
           ((eql ak aj+1)
            (cons rlpui
                  (cons coe ; cht de signature
                        (cons (1+ (caddr poule)) ; multiplicite + 1
                              (cdddr poule)))))
; on doit calculer epsilon_J et z_J ou J= aj+1^{mj+1} U I
; et p_J = paj+1^{mj+1}*p_I :
           (t (let ((nxrlpui (flet ((franz.nthcdr (ind lis)
                                     "equivalent to Franz Lisp 'nthcdr'."
                                     (let ((evalind (eval ind)))
                                       (if (minusp evalind)
                                        (cons nil lis)
                                        (nthcdr evalind lis)))))
                               (franz.nthcdr
                                   (- (car rlpui) aj+1)
                                   (cdr rlpui))))
                    (mj+1 (nth 2 poule)))
                (list (cons aj+1 nxrlpui) ;avant derniere part
                      ; calcul du coefficient
                      (* coe (expt aj+1 mj+1) (factorielle mj+1)) 
                      1  ; ak aurra une multiplicite >=1
                      ak
                      ($mult_sym ($exp_sym (car nxrlpui) ;p(a(j+1))
                                           mj+1) 
                          (car (last poule))))))))))) ;p_I

;................... AVEC LA FORMULE DE GIRARD ....................


(defun girard_pui2ele (max lpui)
  (let ((card (car lpui)) (rlelem (list (cadr lpui) (car lpui))))
    (if (< card max) ; forcement numerique (cf $debut)
        (nconc ($p_calbaselem 2 card rlelem lpui)
               (make-list
                   (- max card)
                   :initial-element 0))
        ($p_calbaselem 2 max rlelem lpui))))

(defun $elereduc0 (min max lelem lpui)
  (cons '(mlist) (elereduc min max (cdr lelem) (cdr lpui))))

(defun elereduc (min max lelem lpui)
  ($troncelem min max lelem ($debut '$p lpui max)))

;bug!!!!!
(defun $troncelem (min max lelem lpui)
  (let ((card (car lpui)))
    (if (< card max)
        (if (< card min)
            (nconc lelem
                   (make-list
                       (1+ (- max min))
                       :initial-element 0)) ;bug!!!!!
            (nconc ($p_baselem min card lelem lpui)
                   (make-list
                       (- max card)
                       :initial-element 0)))
        ($p_baselem min max (cons (car lpui) (cdr lelem)) lpui))))

(defun $p_baselem (min max lelem lpui)
  (let ((l1 (list-length lelem)))
    (if (or (eql 0 l1) (eql 1 l1))
        (flet ((franz.nthcdr (ind lis)
                   "equivalent to Franz Lisp 'nthcdr'."
                   (let ((evalind (eval ind)))
                     (if (minusp evalind) (cons nil lis)
                         (nthcdr evalind lis)))))
          (franz.nthcdr min
              (apply '$p_calbaselem
                     ($ep_rallonge 2 min max
                         (list (car lpui) (cadr lpui)) lpui))))
        (flet ((franz.nthcdr (ind lis)
                   "equivalent to Franz Lisp 'nthcdr'."
                   (let ((evalind (eval ind)))
                     (if (minusp evalind) (cons nil lis)
                         (nthcdr evalind lis)))))
          (franz.nthcdr min
              (apply '$p_calbaselem
                     ($ep_rallonge l1 min max lelem lpui)))))))

(defun $p_calbaselem (min max rlelem lpui) ;m>=2
  (let ((lpui (chsigne lpui)))
    (do ((rlelem rlelem)
         (m min
            (1+ m)))
        ((< max m))
      (flet ((franz.attach (newelt oldlist)
                 "equivalent to Franz Lisp 'attach'."
                 (progn
                   (rplacd oldlist (cons (car oldlist) (cdr oldlist)))
                   (rplaca oldlist newelt))))
        (franz.attach
            ($divi_sym ($p_calelem rlelem lpui (nth m lpui)) m) rlelem)))
    (nreverse rlelem)))

(defun $p_calelem (rlelem lpui en)
  (do ((j 1
          (1+ j))
       (base (cdr lpui) (cdr base)) (rbase rlelem (cdr rbase)) (en en))
      ((null (cdr rbase)) en)
    (setq en ($add_sym en ($mult_sym (car base) (car rbase))))))


; il manque des ei de i=l2 a max
(defun $ep_rallonge (l1 min max list1 list2)
  (let ((l2 (list-length list2)))
    (list (max min l1) max
          (if (< l1 min)
                ; il manque les pi de i=l1 a min-1
              ($complbase '$e (reverse list1) l1 min) (reverse list1))
          (if (or (< l2 max)
                  (eql l2 max))
               ; il manque des ei de i=l2 a max
              (nreverse
                  ($complbase '$p (reverse list2) l2
                      (1+ max)))
              list2))))

;-------------------------------------------------------------------------
;            OBTENIR LES PREMIERES FONCTIONS COMPLETES
; p.16 du Macdonald : h_0 = e_0 = 1  , 
; n*h_n = somme des  p_r * h_{n-r} pour tout r = 1 a n 
; lpui = liste des puissances
; lcomp = liste des completes
;-------------------------------------------------------------------------

(defun $pui2comp_init (max $lpui)
  (cons '(mlist) (pui2comp_init max (cdr $lpui))))

(defun pui2comp_init (max lpui)
  (let* ((lpui ($debut '$p lpui max)) (l (list-length lpui)))
    (pui2comp max
        (if (< l (1+ max))
            (nreverse
                ($complbase '$p (reverse lpui) l
                    (1+ max)))
            lpui))))
; si il n'y a rien a completer dans lpui

(defun pui2comp (max lpui)
  (p_calbasecomp (car lpui) 2 max (list (cadr lpui) (car lpui)) lpui))

; on utilise la meme fonction pour le passage des puissances aux 
; elemantaires : p_calelem

(defun p_calbasecomp (card min max rlcomp lpui)
           ;m>=2 les plus grands pm sont devant
  (do ((rlcomp rlcomp)
       (m min
          (1+ m)))
      ((< max m))
    (flet ((franz.attach (newelt oldlist)
               "equivalent to Franz Lisp 'attach'."
               (progn
                 (rplacd oldlist (cons (car oldlist) (cdr oldlist)))
                 (rplaca oldlist newelt))))
      (franz.attach ($divi_sym ($p_calelem rlcomp lpui (nth m lpui)) m)
          rlcomp)))
  (nreverse rlcomp))

;---------------------------------------------------------------------------
;                A PARTIR DES FONCTIONS SYMETRIQUES COMPLETES
;---------------------------------------------------------------------------
;           OBTENIR LES PREMIERES FONCTIONS SYMETRIQUES ELEMENTAIRES
; CF. ele2comp

(defun $comp2ele_init (max $lcomp)
  (cons '(mlist) (comp2ele_init max (cdr $lcomp))))

(defun comp2ele_init (max lcomp)
  (let* ((lcomp ($debut '$h lcomp max)) (l (list-length lcomp)))
    (comp2ele max
        (if (< l (1+ max))
            (nreverse
                ($complbase '$h (reverse lcomp) l
                    (1+ max)))
            lcomp))))

(defun comp2ele (max lcomp)
  (let ((card (car lcomp)) (rlelem (list (cadr lcomp) (car lcomp))))
    (if (< card max); forcement numerique (cf $debut)
        (nconc (c_calbaselem 2 card rlelem lcomp)
               (makelist
                   (- max card)
                    :initial-element 0))  
        (c_calbaselem 2 max rlelem lcomp))))

; On utilise la fonction $p_calelem du passage des elementaires
; aux puissances.

(defun c_calbaselem (min max rlelem lcomp)
            ;m>=2
  (let ((lcomp (chsigne lcomp)))
    (do ((rlelem rlelem)
         (m min
            (1+ m)))
        ((< max m))
      (flet ((franz.attach (newelt oldlist)
                 "equivalent to Franz Lisp 'attach'."
                 (progn
                   (rplacd oldlist (cons (car oldlist) (cdr oldlist)))
                   (rplaca oldlist newelt))))
        (franz.attach ($p_calelem rlelem lcomp (nth m lcomp)) rlelem)))
    (nreverse rlelem)))
;______________________________________________________________________
;           OBTENIR LES PREMIERES FONCTIONS PUISSANCES
; CF. pui2comp
;______________________________________________________________________

(defun $comp2pui_init (max $lcomp)
  (cons '(mlist) (comp2pui_init max (cdr $lcomp))))

(defun comp2pui_init (max lcomp)
  (let* ((lcomp ($debut '$h lcomp max)) (l (list-length lcomp)))
    (comp2pui max
        (if (< l (1+ max))
            (nreverse
                ($complbase '$h (reverse lcomp) l
                    (1+ max)))
            lcomp))))

(defun comp2pui (max lcomp)
  (let ((card (car lcomp)) (rlpui (list (cadr lcomp) (car lcomp))))
    (c_calbasepui 2 max rlpui lcomp)))

; On utilise la fonction $p_calelem du passage des puissances
; aux elementaires.

(defun c_calbasepui (min max rlpui lcomp)
        ;m>=2
  (let ((-rlpui (mapcar '$moins_sym rlpui)))
    (do ((-rlpui -rlpui)
         (m min
            (1+ m)))
        ((< max m))
      (flet ((franz.attach (newelt oldlist)
                 "equivalent to Franz Lisp 'attach'."
                 (progn
                   (rplacd oldlist (cons (car oldlist) (cdr oldlist)))
                   (rplaca oldlist newelt))))
        (franz.attach
            ($moins_sym
                ($p_calelem -rlpui lcomp ($mult_sym m (nth m lcomp))))
            -rlpui)))
    (nreverse (mapcar '$moins_sym -rlpui))))

;----------------------------------------------------------------------------
;                       Fonctions en commun
; tenir compte du cardinal de l'alphabet lorsque l'on doit completer
; sur les elementaires on ne completera que jusqu'a ce
; cardinal .

(defun $complbase (base rlist i sup)
  (let ((card (car (last rlist))))
    (if (and (equal '$e base)
             (< card (1- sup))) ;forcement numerique
        ($complbase2 base rlist i
            (1+ card))
        ($complbase2 base rlist i sup))))

(defun $complbase2 (base rlist i sup)
  (if (eql i sup) rlist
      ($complbase base
          (cons (flet ((franz.concat (&rest args)
                           "equivalent to Franz Lisp 'concat'."
                           (values (intern (format nil "~{~A~}" args)))))
                  (franz.concat base i))
                rlist)
          (1+ i)
          sup)))

(defun $debut (base list max)
  (let ((card (if (numberp (car list)) (car list) max)))
    (if (or (null list) (null (cdr list)))
        (list card
              (flet ((franz.concat (&rest args)
                         "equivalent to Franz Lisp 'concat'."
                         (values (intern (format nil "~{~A~}" args)))))
                (franz.concat base 1)))
        (cons card (cdr list)))))

(defun chsigne (liste)
  (let ((test t))
    (mapcar #'(lambda (b)
               (if (setq test (not test)) b ($mult_sym -1 b)))
            liste)))
