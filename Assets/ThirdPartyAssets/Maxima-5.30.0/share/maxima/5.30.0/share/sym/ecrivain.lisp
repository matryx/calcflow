; ecrivain.lsp

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

(in-package :maxima)
(macsyma-module ecrivain)

(mdefprop $distri_ecrit
    ((lambda nil) 
     ((mlist) $distribue $lvar)
     ((mprog) (($operation)) (($distri_ecrit_init) $distribue $lvar)))
    mexpr)
(add2lnc '(($distri_ecrit) $distribue $lvar) $functions)

(defun $distri_ecrit_init ($distribue $lvar)
 (if (null (cdr $distribue)) 0
  (if (equal 'mlist (caar $distribue))
      ($ecrivain_sym (mapcar 'cdr (cdr $distribue)) (cdr $lvar))
      ($ecrivain_sym $distribue $lvar))))

(defun $ecrivain_sym (ppart lvar)
  (cond
    ((null ppart) 0)
    ((null (cdr ppart)) (ecrit_mon (cdar ppart) lvar (caar ppart)))
    (t ($fadd_sym
             (mapcar #'(lambda (tpart)
                         (ecrit_mon (cdr tpart) lvar (car tpart)))
                      ppart)))))

(defun ecrit_mon (part lvar coe)
  (cond
    ((null part) coe)
    (t ($mult_sym coe ($fmult_sym (mapcar '$exp_sym lvar part))))))
