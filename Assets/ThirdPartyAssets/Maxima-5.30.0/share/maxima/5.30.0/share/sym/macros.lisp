; Fichier macros.lsp

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

(defvar COEI)
(defvar TERMREST)
(defvar CHCOETERM)
(defvar MONI)
(defvar A)
(defvar TCOE)
(defvar B)
(defvar TMON)
(defvar C)
(defvar LGI)
(defvar D)
(defvar TLG)
(defvar SYM)
(defvar TERM)
(defvar 3CONS)
(defvar TERMI)
(defvar P)
(defvar COE)
(defvar 4CONS)





