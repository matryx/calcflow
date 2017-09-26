; treillis.lsp

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
;               ALGORITHMES SUR LES PARTITIONS
;============================================================================
;                        INTERFACE

(in-package :maxima)
(macsyma-module treillis)

(mdefprop $treillis
    ((lambda ()) ((mlist) $poids)
     ((mprog) (($operation)) (($treillis_init) $poids)))
    mexpr)
(add2lnc '(($treillis) $poids) $functions)
(mdefprop $lgtreillis
    ((lambda ()) ((mlist) $poids $longueur)
     ((mprog) (($operation)) (($lgtreillis_init) $poids $longueur)))
    mexpr)
(add2lnc '(($lgtreillis) $poids $longueur) $functions)

(mdefprop $ltreillis
    ((lambda ()) ((mlist) $poids $longueur)
     ((mprog) (($operation)) (($ltreillis_init) $poids $longueur)))
    mexpr)
(add2lnc '(($ltreillis) $poids $longueur) $functions)

;==========================================================================
;              TREILLIS DES PARTITIONS DE POIDS n
;                   dans l'ordre lexicographique
(defun $treillis_init (n)
  (macsy_list (treillis n)))

(defun treillis (n)
  (let ((ltreillis (cons nil nil)))
    (treillis2 n 0 ltreillis nil)
    (cdr ltreillis)))

; p(i+1)^m=max(0,2pi-p(i+1))
; p(i+1)^M = pi-1
(defun treillis2 (pui ote ltreillis poule)
  (cond
    ((eql 0 pui) (rplacd ltreillis (list (reverse poule))))
    (t (treillis2 ote
           (max 0
                 (- (* 2 ote)
                           pui))
           ltreillis
           (cons (- pui ote)
                 poule))
       (let ((ote (1+ ote)))
         (and (< ote pui)
              (treillis2 pui ote (last ltreillis) poule))))))
;=========================================================================
;              TREILLIS DES PARTITIONS DE POIDS ET LONGUEUR FIXE
;                   dans l'ordre lexicographique

(defun $lgtreillis_init (p l)
    (macsy_list (lgtreillis p l)))

(defun lgtreillis (poids longueur)
  (let ((lpart (cons nil nil)))
       (lgtreillis2 poids
                    (- longueur 1) 
                    (- longueur 1) 
                    (maxote poids longueur) nil lpart)
    (cdr lpart)))

(defun lgtreillis2 (poids rlongueur ote maxote partition lpart)
  (cond ((minusp rlongueur)
         (rplacd lpart (list (reverse partition))))
       (t (lgtreillis2 ote
                      (- rlongueur 1)
                      (max (- rlongueur 1)  (- (* 2 ote) poids))
                      (maxote ote rlongueur)
                      (cons (- poids ote)
                            partition)
                      lpart)
            (and (< ote maxote)
                 (lgtreillis2 poids
                         rlongueur 
                         (1+ ote)
                         maxote
                         partition
                         (last lpart))))))

; la fonction maxote est commune a : treillis.lsp , resolvante.lsp, kak.lsp
; voir dans util.lsp

;-----------------------------------------------------------------------
; PARTITIONS DE POIDS FIXE ET DE LONGUEUR BORNEE

(defun $ltreillis_init (p l)
    (macsy_list (ltreillis p l)))

(defun ltreillis (poids longueur)
  (let ((lpart (cons nil nil)))
    (ltreillis2 poids
               (- longueur 1)
        0 (maxote poids longueur) nil lpart)
    (cdr lpart)))

(defun ltreillis2 (poids rlongueur ote maxote partition lpart)
  (cond ((minusp rlongueur)
         (rplacd lpart (list (reverse partition))))
        (t (ltreillis2   ote
                         (- rlongueur 1)
                         (max 0 (- (* 2 ote) poids))
                         (maxote ote rlongueur)
                         (cons (- poids ote) partition)
                         lpart)
            (and (< ote maxote)
                   (ltreillis2 poids
                         rlongueur 
                         (1+ ote)
                         maxote
                         partition
                         (last lpart))))))




