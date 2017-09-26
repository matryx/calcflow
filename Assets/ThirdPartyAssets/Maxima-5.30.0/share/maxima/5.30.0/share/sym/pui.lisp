; Fichier pui.lsp

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

;==========================================================================
;                      PASSAGE DES FONCTIONS PUISSANCES
;                           AUX FORMES MONOMIALES
; appel avec pui([card,p1,...,pn],sym(x,y,..,z),[x,y,...,z])
;==========================================================================
;                     INTERFACE

(in-package :maxima)
(macsyma-module pui macros)

(mdefprop $pui
    ((lambda ()) ((mlist) $valpi $sym $lvar)
     ((mprog) (($operation)) (($pui_init) $valpi $sym $lvar)))
    mexpr)
;; IT APPEARS ARGS WAS A MACRO. THERE IS NO ARGS MACRO AT PRESENT.
;; DUNNO IF THE ABSENCE OF ARGS CAUSES ANY INCORRECT BEHAVIOR IN SYM
;; (args $pui '(3 . 3))
(add2lnc '(($pui) $valpi $sym $lvar) $functions)
(mdefprop $multi_pui
    ((lambda ()) ((mlist) $lvalpi $pc $llvar)
     ((mprog) (($operation)) (($multi_pui_init) $lvalpi $pc $llvar)))
    mexpr)
(add2lnc '(($multi_pui) $lvalpi $pc $llvar) $functions)

; fonction bidon de chargement pour eviter de construire pour detruire
; lorsque l'on appelle une fonction de npui a partir d'un autre
; fichier du module sym
(defun $bidon2 ())
;==========================================================================
;               DECLARATIONS AU COMPILATEUR
(progn (defvar listpi) (defvar $pui) (defvar $testpartpol))


;; Les macros
; inclusion du fichier macros.lsp

;(progn
 ; (compile-file "//users//avb//symetrique//sym//symakcl//maxima//macros.o")
  ;(load "//users//avb//symetrique//sym//symakcl//maxima//macros.o"))
;---------------------------------------------------------------------------
;                     DECLARATION DES MACROS
; pour le type 2 des polynomes partitionnes avec en tete de chaque
; terme partitionne sa longueur
;---------------------------------------------------------------------------

(defmacro lgi (sym) (list 'caar sym)); longueur de la partition initiale
(defmacro moni (p) (list 'cddar p)); partition initiale
(defmacro coei (p) 
          (list 'cadar p)); coefficient associe a la partition initiale
(defmacro termi (p) (list 'car p)); terme partitionne initial
(defmacro chcoeterm (term coe); modification physique du coefficient d'un terme
          (list 'progn (list 'rplaca (list 'cdr term) coe) term))
(defmacro termrest (p) 
          (list 'cdr p)); liste de  termes partitionnes sans le premier
(defmacro tmon (term) (list 'cddr term)); partition d'un terme partitionne
(defmacro tcoe (term) (list 'cadr term)); coefficient d'un terme partitionne
(defmacro tlg (term) 
         (list 'car term)); longueur de la partition d'un terme partitionne

;--------------------------------------------------------------------------
;                      MULTIDECOMPOSITION
;--------------------------------------------------------------------------

(defun $multi_pui_init ($multi_lpui $multi_pc $llvar)
  (multi_pui  (cdr $multi_lpui) $multi_pc
                      (cdr $llvar)))
;cf. p_red1
(defun multi_pui (multi_lpui $multi_pc l$lvar)
  (cond
    ((meval (list '($is) (list '(mequal) $multi_pc 0))) 0)
    ((null l$lvar) $multi_pc)
    (t (multi_pui (cdr multi_lpui)
              (if (meval (list '($is) (list '(mequal) $multi_pc 0))) 0
                 (p_red1 (car multi_lpui)
; on a le polynome multisymetrique sous forme contracte'
; on considere qu'il est symetrique en un bloc de variables, les
; autres intervenant dans les coefficients
; on ramene sa forme partitionnee avec les longueurs devant
                           (lgparts (ch2repol 
                                 (mac2lisp (meval 
                                               (list '($cont2part) $multi_pc
                                                      (car l$lvar))))))))
                            
           (cdr l$lvar)))))
;***************************************************************************
;          MISE SOUS FORME INTERNE DU POLYNOME SYMETRIQUE 
;                SUIVANT LES FORMES EXTERNES DONNEES
; Donnees :
; valpi = ((mlist) card p1 ....)  ou card est le cardinal
; sym est un polynome symetrique pouvant etre represente 
; de plusieurs manieres en entree .
; lvar = ((mlist) x1 x2 ...) les variables de sym.
; Representation interne : REP([pol]) = [lppart](2)
;                          listpi=(card p1 ...)
;----------------------------------------------------------------------------
; sym = polynome contracte
;le polynome symetrique en entier ou en partie
; sym=REP([pol])(1)
; sym est le polynome symetrique 
; on test egalement sa symetrie
; sym = (REP([pol])(2) + longueurs) retirer les "mlist"
; sym = REP([pol])(2)
(defun $pui_init (valpi sym $lvar)
  (let ((sauvlistpi
            (cdr (flet ((franz.boundp (name)
                            "equivalent to Franz Lisp 'boundp'."
                            (and (boundp name)
                                 (cons nil (symbol-value name)))))
                   (franz.boundp 'listpi)))))
    (prog1 (case $pui
             (1
              (if (meval (list '($is) (list '(mequal) sym 0))) 0
                  (p_red1  valpi                      
                      (lgparts (ch2repol 
                                  (mac2lisp (meval 
                        (list '($cont2part) sym $lvar))))))))
             (2
              (if (meval (list '($is) (list '(mequal) sym 0))) 0
              (p_red1 valpi
                      (lgparts (ch2repol 
                                 (mac2lisp (meval
                           (list '($partpol) sym $lvar))))))))     
             (3
              (p_red1 valpi
                      (lgparts (ch2repol
                                   (mapcar 'cdr (cdr sym))))))
             (4
              (let ((pol (lgparts (ch2repol
                                    (mac2lisp (meval
                      (list '($tpartpol) sym $lvar)))))))
                (p_red2 ($degrep pol) pol  valpi)))
             (5 (p_red1  valpi (mapcar 'cdr (cdr sym))))
             (6 (p_red1 valpi (lgparts (mapcar 'cdr (cdr sym)))))
             (t "erreur $pui n'a pas de valeur"))
      (setq listpi sauvlistpi))))
;**************************************************************************

(defun p_red1 ($l ppart) 
   (p_red2 ($degrep ppart)
            (sort ppart 'orlongsup) $l))

; on n'a qu'une constante	
; dans fichier chbase
(defun p_red2 (degpol ppart $l) 
  (cond
    ((eql 0 (lgi ppart)) (coei ppart))
    (t (setq listpi (cdr (meval (list '($puireduc) degpol $l)) ))
       ($p_reduit ppart))))
;--------------------------------------------------------------------------
;                 LA BOUCLE PRINCIPALE         
;--------------------------------------------------------------------------
;On rajoute la fonction puifor2 a l'interieur
(defun $p_reduit (sym)
  (cond
    ((or (null sym) (eql 1 (lgi sym))) (p_ecrit sym))
    ((eql 2 (lgi sym)) (longde2 sym 0))
    (t ($p_reduit
           (somme (termrest sym)
                  (p_reducpart (moni sym) (coei sym) (lgi sym))
                  'orlongsup)))))
; Pour le fichier kak uniquement :
(defun $p_reduit_init ($sym)
     ($p_reduit (mapcar 'cdr (cdr $sym))))
;-------------------------------------------------------------------------
; Calcul des fonctions puissances pour des partitions de longueur 2
;m(i)! * \sum x^i y^j = \sum x^i \sum x^j - \sum x^{i+j}
;              = p_i*p_j - p_{i+j}
; ici la multiplicite m(i) de i dans \sum x^i y^j est de 1 ou 2
;-------------------------------------------------------------------------
; m(i)=1=m(j)
(defun longde2 (sym lg2) 
  (cond
    ((or (null sym) (eql 1 (lgi sym))) ($add_sym lg2 (p_ecrit sym)))
    (t (let ((partin (moni sym)))
         (longde2 (cdr sym)
                  ($add_sym lg2
                      ($mult_sym (coei sym)
                          (if (eql 4 (list-length partin))
                              (let ((i (car partin))
                                    (j (caddr partin)))
                                ($add_sym
                                    ($moins_sym
                                     (nth
                                      (+ i j)
                                      listpi))
                                    ($mult_sym (nth j listpi)
                                     (nth i listpi))))
                              ($divi_sym
                                  ($add_sym
                                      ($moins_sym
                                       (nth
                                        (* 2 (car partin))
                                        listpi))
                                      ($exp_sym
                                       (nth (car partin) listpi) 2))
                                  2)))))))))
;-------------------------------------------------------------------------
;              REECRITURE D'UNE FORME MONOMIALE
;          EN FONCTION DE FORMES MONOMIALES INFERIEURES
;                 POUR L'ORDRE DES LONGUEURS
; CAS r=1 (cf. article)
;-------------------------------------------------------------------------
; mapc agit sur tout les car successifs de ses listes arguments et rends la
; premiere liste (la seule ici et que l'on modifie physiquement).
; Comme on n'a plus besoin de part on peut la modifier physiquement
;--------------------------------------------------------------------------
(defun p_reducpart (part coe lg)
  (let* ((puim (car part)) (m (cadr part))
         (-m (* -1 m))
         (coef (nth puim listpi)) (partf (p_fact part m))) 
    (mapc #'(lambda (tpart)
             (flet ((franz.attach (newelt oldlist)
                        "equivalent to Franz Lisp 'attach'."
                        (progn
                          (rplacd oldlist
                                  (cons (car oldlist) (cdr oldlist)))
                          (rplaca oldlist newelt))))
               (franz.attach (1- lg)
                             tpart)))
          (constsol coef m coe partf
              (mapc #'(lambda (part)
                       (flet ((franz.attach (newelt oldlist)
                                  "equivalent to Franz Lisp 'attach'."
                                  (progn
                                    (rplacd oldlist
                                     (cons (car oldlist) (cdr oldlist)))
                                    (rplaca oldlist newelt))))
                         (franz.attach ($divi_sym coe -m) part)))
                    (multpui partf puim))))))
; si la puim-ieme fonction puissance est non nulle on rajoute partf
; aux nouvelles partitions a decomposer.
(defun constsol (coef m coe partf prsol)
  (if (and (numberp coef)(zerop coef)) prsol
      (nconc prsol
             (list (cons ($divi_sym ($mult_sym coef coe) m) partf)))))
; On va eventuellement modifier physiquement part
(defun p_fact (part m)
  (cond 
    ((eql 1 m) 
     (cddr part))
    (t
     (rplaca (cdr part) (1- m))
     part)))
;---------------------------------------------------------------------------
;             PRODUIT D'UNE FORME MONOMIALE parf PAR 
;              LA FONCTION PUISSANCE DE POIDS puim.
; partf a pour representation [partition](2)
; LE CAS r=1 PERMET DE N'AVOIR QUE DES COEFFICIENTS EGAUX A 1.
;-----------------------------------------------------------------------
(defun multpui (partf puim)
  (let ((k (cons nil nil))) (multpui2 partf puim nil k) (cdr k)))
; les partitions sont rangees dans l'ordre lexicographique decroissant
; dans k. Etant de meme longueur on les obtiend donc dans l'ordre
; des longueurs decroissant.
; nconc impossible ici
(defun multpui2 (part puim s k)
  (or (null part)
      (let ((pui (car part)) (nb (cadr part)) (rpart (cddr part)))
        (multpui2 rpart puim (append s (list pui nb))
            (cdr (rplacd k
                         (list (list* (+ pui puim)
                                      1
                                      (nconc s
                                       (restpart pui
                                        (1- nb)
                                        rpart))))))))))
(defun restpart (pui nb part)
  (if (eql 0 nb) part (cons pui (cons nb part))))
;----------------------------------------------------------------------------
;                      L'ECRIVAIN
;----------------------------------------------------------------------------
; une constante
(defun p_ecrit (solu)
  (let ((solu (nreverse solu)))
    (cond
      ((null solu) 0)
      ((eql 0 (lgi solu))
       (p_ecrit2 (cdr solu) (cdr listpi) (coei solu) 1))
      (t (p_ecrit2 solu (cdr listpi) 0 1)))))
(defun p_ecrit2 (solu listpi mpol i_init)
  (let ((i (car (moni solu))))
    (cond
      ((null solu) mpol)
      ((eql i i_init)
       (p_ecrit2 (cdr solu) listpi
           ($add_sym mpol ($mult_sym (coei solu) (car listpi))) i_init))
      (t (setq listpi
               (flet ((franz.nthcdr (ind lis)
                          "equivalent to Franz Lisp 'nthcdr'."
                          (let ((evalind (eval ind)))
                            (if (minusp evalind) (cons nil lis)
                                (nthcdr evalind lis)))))
                 (franz.nthcdr
                     (- i i_init)
                     listpi)))
         (p_ecrit2 (cdr solu) listpi
             ($add_sym mpol ($mult_sym (coei solu) (car listpi))) i)))))
