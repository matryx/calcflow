; Fichier elem.lsp

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
;               DECOMPOSITION D'UN POLYNOME SYMETRIQUE
;                   PAR LES SYMETRIQUES ELEMENTAIRES

; appelle avec elem([card,e1, e2, ...],sym(x,y,..,z),[x,y,...,z])
; ou multi_elem pour des polyn\^omes multisym\'etriques
;=============================================================================
(in-package :maxima)
(macsyma-module elem macros)



(mdefprop $elem
    ((lambda ()) ((mlist) $valei $sym $lvar)
     ((mprog) (($operation)) (($elem_init) $valei $sym $lvar)))
    mexpr)

;; IT APPEARS ARGS WAS A MACRO. THERE IS NO ARGS MACRO AT PRESENT.
;; DUNNO IF THE ABSENCE OF ARGS CAUSES ANY INCORRECT BEHAVIOR IN SYM
;; (args $elem '(3 . 3))

(add2lnc '(($elem) $valei $sym $lvar) $functions)

(mdefprop $multi_elem
    ((lambda ()) ((mlist) $lvalei $pc $llvar)
     ((mprog) (($operation)) (($multi_elem_init) $lvalei $pc $llvar)))
    mexpr)

(add2lnc '(($multi_elem) $lvalei $pc $llvar) $functions)

;================================================================
; fonction bidon de chargement pour eviter de construire pour detruire
; lorsque l'on appelle une fonction de elem a partir d'un autre
; fichier du module sym
(defun $bidon ())
;---------------------------------------------------------------------------
;           VARIABLES DECLAREES SPECIALES PAR LE COMPILATEUR
(progn
  (defvar listei)
  (defvar $elem)
  (defvar nb1)
  (defvar lgI)
  (defvar coei)
  (defvar nblib))

;; Les macros
; inclusion du fichier macros.lsp
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

;***************************************************************************
;          MISE SOUS FORME INTERNE DU POLYNOME SYMETRIQUE 
;                SUIVANT LES FORMES EXTERNES DONNEES
; Donnees :
; valei = ((mlist) card e1 e2 ...)   
; sym est un polynome symetrique pouvant etre represente 
; de plusieurs manieres en entree .
; lvar = ((mlist) x1 x2 ...) les variables de sym.
; Representation interne : REP([pol]) = [lppart](2)
;                          listei=(card e1 e2 ...)

;----------------------------------------------------------------------------
;                MULTIDECOMPOSITION
; Le polynome donne est multi-symetrique sous forme contractee
;----------------------------------------------------------------------------
(defun $multi_elem_init ($multi_lelem $multi_pc $llvar)
  (multi_elem (mapcar 'cdr (cdr $multi_lelem)) $multi_pc
              (cdr $llvar)))

; cf. e_red1 plus loin

(defun multi_elem (multi_lelem $multi_pc l$lvar)
  (cond
    ((meval (list '($is) (list '(mequal) $multi_pc 0))) 0)
    ((null l$lvar) $multi_pc)
    (t (multi_elem (cdr multi_lelem)
              (if (meval (list '($is) (list '(mequal) $multi_pc 0))) 0
                  (e_red1 (car multi_lelem)
                          (lgparts (ch2repol 
                                 (mapcar 'cdr
                                         (cdr (meval 
                         (list '($cont2part) $multi_pc
                                         (car l$lvar)))))))))
           (cdr l$lvar)))))

;---------------------------------------------------------------------------


(defun $elem_init (valei sym $lvar)
  (let ((sauvlistei
            (cdr (flet ((franz.boundp (name)
                            "equivalent to Franz Lisp 'boundp'."
                            (and (boundp name)
                                 (cons nil (symbol-value name)))))
                   (franz.boundp 'listei)))))
    (prog1 (case $elem
             (1 ; sym = polynome contracte 
              (if (meval (list '($is) (list '(mequal) sym 0))) 0
                  (e_red1 (cdr valei) 
                      (lgparts (ch2repol 
                                  (mac2lisp (meval 
                        (list '($cont2part) sym $lvar))))))))
             (2 ;le polynome symetrique en entier ou en partie
              (if (meval (list '($is) (list '(mequal) sym 0))) 0
                  (e_red1 (cdr valei)
                      (lgparts (ch2repol 
                                 (mac2lisp (meval
                           (list '($partpol) sym $lvar))))))))
             (3 ; sym=REP([pol])(1) mais pas forcement ordonne'
		; mais les monomes sont tous suppose's distincts 
              (e_red1 (cdr valei) 
                      (lgparts (ch2repol (mapcar 'cdr (cdr sym))))))
             (4 ; sym est le polynome symetrique 
                ; on test egalement sa symetrie
              (let ((pol (lgparts (ch2repol
                                    (mac2lisp (meval
                      (list '($tpartpol) sym $lvar)))))))
                (e_red2 ($degrep pol) pol (cdr valei) )))
             (5 ; sym = (REP([pol])(2) + longueurs) retirer les "mlist"
               (e_red1 (cdr valei) (mapcar 'cdr (cdr sym))))
             (6 ; sym = REP([pol])(2)
               (e_red1 (cdr valei) (lgparts (mapcar 'cdr (cdr sym)))))
             (t "erreur $elem n'a pas de valeur"))
      (setq listei sauvlistei))))

(defun e_red1 (l ppart) 
  (e_red2 ($degrep ppart)
          (sort ppart '$e_lexinv) l))


(defun e_red2 (degpol ppart l)
  (cond
    ((eql 0 (lgi ppart)) (coei ppart)) ; on n'a qu'une constante
    (t (setq listei
             (rangei l
                     (if (and l (numberp (car l))) 
                         (min (car l) degpol) ; le cardinal est impose
                         degpol)
                     (list-length l)))
         ; autant que l'inf du cardinal  et du degre du polynome
       ($reduit (if (numberp (car l)) (car l) degpol) ppart))))

;---------------------------------------------------------------------------
;           CREATION DE LA LISTE listei DES VALEURS DES ELEMENTAIRES
;l=(card e1 e2 ... e(lg))  card est le cardinal de l'alphabet.
; avec ki < k(i+1)
;----------------------------------------------------------------------------
; on range les plus grand en premier

(defun rangei (l n lg)
  (if (eql (1+ n) lg)
      l (append l (rangei2 nil lg n))))

(defun rangei2 (lesei i n)
  (if (< n i)
      (nreverse lesei) 
      (rangei2 (cons (flet ((franz.concat (&rest args)
                                "equivalent to Franz Lisp 'concat'."
                                (values (intern
                                         (format nil "~{~A~}" args)))))
                       (franz.concat '$e i))
                     lesei)
               (1+ i)
               n)))

;--------------------------------------------------------------------------
;                   LA BOUCLE PRINCIPALE
; sym = [lppart](2) ordonnee dans l'ordre lexicographique decroissant.
;-------------------------------------------------------------------------

(defun $reduit (card sym)
  (let ((I (moni sym)))
    (if (or (null sym) (syele I)) (e_ecrit sym)
        ($reduit card
                 (somme (cdr sym)
                        (devel1 (factpart I)
                                (coei sym) (lgi sym) card)
                        '$e_lexinv)))))
;-------------------------------------------------------------------------
;                        FACTORISATION DE I
;--------------------------------------------------------------------------
(defun factpart (i)
  (let ((test nil) (alt nil))
    (let ((j (mapcar #'(lambda (puiounb)
                        (setq alt (null alt))
                        (if alt
                            (if (eql 1 puiounb)
                                (and (setq test 't) nil)
                                (1- puiounb))
                            puiounb))
                     i)))
      (cond 
	(test
	 (setq nb1 (car (last j)))
	 (nbutlast (nbutlast j)))
	(t
	 (setq nb1 0) j)))))
;---------------------------------------------------------------------------
;                             REECRITURE DE I
;                  Developpement de ei*J ou i= lgI = nb1 + lgJ
;                J=(pui1 n1 pui2 n2 .....) avec puik > pui(k-1)
;----------------------------------------------------------------------------

(defun devel1 (J coeI lgI card)
   (let ((coeJ ($mult_sym coeI (nth lgI listei))) 
               (nblib (- card lgI)))
         (nconc (and (plusp nblib)
                     (devel2 J nblib (cons nil nil)))
                (and (or (not (numberp coeJ)) 
                         (null (zerop coeJ)) )
                     (list (list* (- lgI nb1) coeJ J))))))


(defun devel2 (J nblib pilesol)
   (devel3 pilesol J 0 (cadr J) (cons -1 nil) nil)
   (cddr pilesol)) ; pilesol=(nil I . listparti)

;----------------------------------------------------------------------------
;   r le nombre d'elements passant a la meme puissance superieure, pui1 + 1.
; r vaut n1 au depart et decroit jusqu'a une valeur inf non negative.
; Ou inf est choisie afin que la forme monomiale representee
; par la partition ramenee soit non nulle relativement au cardinal, card, de 
; l'alphabet concidere. En fait il faut que la longueur de cette partition
; qui est de nbpui1 + lgI soit inferieure ou egal a card.
;    solu est la partition partielle d'une partition solution en construction
;    pile contient les partitions en construction mais mise en instance
;    pilesol contient les partition solutions deja construites
;-----------------------------------------------------------------------

(defun devel3 (pilesol J nbpui1 r solu pile) 

   (if (null  J) 
       (progn (rplacd pilesol 
                      (list (ramsolfin nbpui1 (+ nbpui1 nb1) solu)))
              (and pile
                  (devel3 (cdr pilesol); pas apply pour recursivite terminale
                           (car pile)
                           (cadr pile)
                           (caddr pile)
                           (car (cdddr pile))
                           (car (last pile)))))
       (let* ((reste (- (cadr J) r))
              (nnbpui1 (+ nbpui1 reste)))
; on met le cas r --> r-1 en instance (si nnbpui1 + lgI < card) en empilant,
; et on passe tout de suite a r --> n2 pour continuer a construire une 
; partition solution.
             (devel3 pilesol
                     (cddr J)                         ; (pui2 n2 .....)
                     nnbpui1                          ; lg(M) >= nbpui1 + lgI
                     (cadr (cddr J))                  ; n2
                     (ramsol (car J) reste r solu)
                     (if (and (plusp r)
                              (> nblib nnbpui1))      ; **
                         (list J nbpui1 (1- r) solu pile)
                          pile) ))))

; ** pour ne pas depasser le cardinal de l'alphabet

(defun ramsol (pj nbj r solu)
  (if (eql 0 r) (list* (car solu) nbj pj (cdr solu))
      (let ((solu (ramsol2 pj r (car solu) (cdr solu))))
        (if (eql 0 nbj) solu (list* (car solu) nbj pj (cdr solu))))))

(defun ramsol2 (pj r coe solu)
  (if (eql (cadr solu)
             (1+ pj))
      (list* (calcoe coe (car solu) r)
             (+ (car solu) r)
             (cdr solu))
      (list* coe r
             (1+ pj)
             solu)))
; tnb1=0 si sol=I et que nb1=0
(defun ramsolfin (nbpui1 tnb1 solu)
  (if (eql 1 (caddr solu))
      (list* (+ lgI nbpui1)
             ($mult_sym coei (calcoe (car solu) tnb1 (cadr solu)))
             (reverse (cons (+ tnb1 (cadr solu))
                            (cddr solu))))
      (list* (+ lgI nbpui1)
             ($mult_sym coei (car solu))
             (reverse (list* tnb1 1 (cdr solu))))))
;-------------------------------------------------------------------------
;         CALCUL DU COEFFICIENT BINOMIAL C(n+c,c)
;--------------------------------------------------------------------------
(defun calcoe (coe c n)
  (let ((nc (+ n c)))
    (* coe (calcoe2 (inferieur n c) nc
                        (1- nc)
                        2))))

(defun calcoe2 (n res nc i)
  (if (eql (1+ n)
           i)
      res
      (calcoe2 n
               (div (* res nc)
                    i)
               (1- nc)
               (1+ i))))
;---------------------------------------------------------------------------
; syele teste si une partition est celle d'une fonction 
;  symetrique elementaire
(defun syele (mon)
  (and mon (and (eql 1 (car mon)) (null (cddr mon)))))
(defun inferieur (a i) (and a (min a i)))
; ---------------------------------------------------------------------------
;                       L'ECRIVAIN
;----------------------------------------------------------------------------
; une constante
(defun e_ecrit (solu)
  (let ((solu (nreverse solu)))
    (cond
      ((null solu) 0)
      ((eql 0 (lgi solu))
       (e_ecrit2 (cdr solu) (cdr listei) (coei solu) 1))
      (t (e_ecrit2 solu (cdr listei) 0 1)))))
(defun e_ecrit2 (solu listei mpol i_init)
  (let ((i (lgi solu)))
    (cond
      ((null solu) mpol)
      ((eql i i_init)
       (e_ecrit2 (cdr solu) listei
           ($add_sym mpol ($mult_sym (coei solu) (car listei))) i_init))
      (t (setq listei
               (flet ((franz.nthcdr (ind lis)
                          "equivalent to Franz Lisp 'nthcdr'."
                          (let ((evalind (eval ind)))
                            (if (minusp evalind) (cons nil lis)
                                (nthcdr evalind lis)))))
                 (franz.nthcdr
                     (- i i_init)
                     listei)))
         (e_ecrit2 (cdr solu) listei
             ($add_sym mpol ($mult_sym (coei solu) (car listei))) i)))))





