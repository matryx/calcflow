; Fichier operations.lsp

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
;                           OPERATIONS
;=========================================================================

(in-package :maxima)
(macsyma-module operations)

(progn (defvar $oper) (defvar prefixe))
; Aucune fonction n'est locale

;_________________________________________________________________________
;                     Les operations avec rat
(defun $ratmult (a b) (meval (list '($rat) (list '(mtimes) a b))))
(defun $ratadd (a b) (meval (list '($rat) (list '(mplus) a b))))
(defun $ratfmult (ll)
  (meval (list '($rat) (cons '(mtimes) ll))))

(defun $ratfadd (l)
  (meval (list '($rat) (cons '(mplus) l))))
(defun $ratdivi (a b) (meval (list '($rat) (list '(mquotient) a b))))
(defun $ratexp (x n) (meval (list '($rat) (list '(mexpt) x n))))
(defun $ratmoins (a) (meval (list '($rat) (list '(mminus) a))))
;_________________________________________________________________________
;                     Les operations  pour expand
(defun $expandmult (a b)
  (meval (list '($expand) (list '(mtimes) a b))))
(defun $expandadd (a b) (meval (list '($expand) (list '(mplus) a b))))

(defun $expandfmult (ll)
  (meval (list '($expand) (cons '(mtimes) ll))))
(defun $expandfadd (l)
   (meval (list '($expand) (cons '(mplus) l))))
(defun $expanddivi (a b)
  (meval (list '($expand) (list '(mquotient) a b))))
(defun $expandexp (x n) (meval (list '($expand) (list '(mexpt) x n))))
(defun $expandmoins (a) (meval (list '($expand) (list '(mminus) a))))
;_________________________________________________________________________
;                     Les operations avec ratsimp
(defun $ratsimpmult (a b)
  (meval (list '($ratsimp) (list '(mtimes) a b))))
(defun $ratsimpadd (a b)
  (meval (list '($ratsimp) (list '(mplus) a b))))
(defun $ratsimpfmult (ll)
  (meval (list '($ratsimp) (cons '(mtimes) ll))))
(defun $ratsimpfadd (l)
   (meval (list '($ratsimp) (cons '(mplus) l))))
(defun $ratsimpdivi (a b)
  (meval (list '($ratsimp) (list '(mquotient) a b))))
(defun $ratsimpexp (x n)
  (meval (list '($ratsimp) (list '(mexpt) x n))))
(defun $ratsimpmoins (a)
  (meval (list '($ratsimp) (list '(mminus) a))))
;_________________________________________________________________________
;                     Les operations avec meval
(defun $mevalmoins (a) (meval (list '(mminus) a)))
(defun $mevalmult (a b) (meval (list '(mtimes) a b)))
(defun $mevaladd (a b) (meval (list '(mplus) a b)))
(defun $mevalfmult (ll)
  (meval (cons '(mtimes) ll)))
(defun $mevalfadd (l)
   (meval (cons '(mplus) l)))
(defun $mevaldivi (x y) (meval (list '(mquotient) x y)))
(defun $mevalexp (x n) (meval (list '(mexpt) x n)))
;------------------------------------------------------------------------
; INITIALISATION
(setq prefixe 'depart)
;------------------------------------------------------------------------
; CETTE FONCTION PERMET DE CHANGER LE CORPS DES FONCTIONS DY TYPE 
;                       $operateur_sym
; SELON LE CHOIX DU MODE OPERATOIR DEMANDE PAR L'UTILISATEUR
;------------------------------------------------------------------------
; supposons que $oper = $rat
; on met dans $moins $mult ... la lambda de $ratmoins $ratmult ...
; creation de la liste ($ratmoins $ratmult ... $ratfmult)

(defun $operation ()
  (cond
    ((equal $oper prefixe))
    (t (mapc #'(lambda (corps nom_oper)
                (setf (symbol-function nom_oper) corps))
             (mapcar #'(lambda (suffixe)
                        (symbol-function
                            (flet ((franz.concat (&rest args)
                                    "equivalent to Franz Lisp 'concat'."
                                    (values
                                     (intern
                                      (format nil "~{~A~}" args)))))
                              (franz.concat $oper suffixe))))
                     '(moins mult add divi exp fadd fmult))
             '($moins_sym $mult_sym $add_sym $divi_sym $exp_sym
                  $fadd_sym $fmult_sym))
       (setq prefixe $oper))))

;------------------------------------------------------------------------
; LE PREMIER APPEL
;; ($operation)

