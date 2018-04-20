; permut.lsp

;       ***************************************************************
;       *                    MODULE SYM                               *
;       *       MANIPULATIONS DE FONCTIONS SYMETRIQUES                *
;       *        (version01: Commonlisp pour Maxima)                 *
;       *                                                             *
;       *                ----------------------                       *
;       *          Philippe ESPERET Annick VALIBOUZE                  *
;       *                    GDR MEDICIS                              *
;       *  (Mathe'matiques Effectives, De'veloppements Informatiques, *
;       *           Calculs et Ingenierie, Syste`mes)                 *
;       *             LITP (Equipe Calcul Formel)                     *
;       *                 Universite' Paris 6,                        *
;       *        4 place Jussieu, 75252 Paris cedex 05.               *
;       *              e-mail : avb@sysal.ibp.fr                      *
;       ***************************************************************

;---------------------------------------------------------------------------
;           OBTENIR TOUTES LES PERMUTATIONS D'UN NUPLET D'ENTIER
;---------------------------------------------------------------------------
; appel : $permut
;========================================================================
;                        DECLARATIONS AU COMPILATEUR
;-------------------------------------------------------------------------

(in-package :maxima)
(macsyma-module permut)

(progn (defvar lvar) (defvar permut))
; LES PERMUTATIONS
; $permut 
;** FTOC. WARNING:
;             Franz Lisp declaration 'localf' is currently untranslated
(progn)
(defun $permut (nuplet)
  (cons '(mlist)
        (mapcar #'(lambda (permu) (cons '(mlist) permu))
                (permut (cdr nuplet)))))
;cas particulier de permut avec les elem. de L 2 a 2 <>(pour voir) 
;On place le car
; dans toutes les positions possibles 
;(de permut0(L)  
; (let (    (i 0) (reponse ()) (relais ())   ) 
;  (if (<= (length L) 1) 
;      (list L)
;     (setq relais (permut0 (cdr L))
;             a        (car L))
;     (while (< i (length L))
;      (setq reponse
;            (append reponse 
;                   (append (mapcar 
;                           '(lambda(z)
;                                (insert0 a z i))
;                          relais)))
;              i (+ 1 i)))
;     reponse)))
; pour le cas ge'ne'ral c'est pareil sauf que j'augmente i assez 
; a' chaque etape
; pour etre sur qu'il  y a moins de re'pe'tition : j'en obtiens encore trop
; ne'anmoins, ce surplus est du a' : 
; des () qui viennent la'-dedans a' cause de insert : je les ote avec vire_nil
; des re'pe'titions malgre_tout:  je vais les e'liminer par un_de_chaque()
(defun insertion (a l i)
  (if (null l) (list a)
      (if (equal (nth i l) a) nil
          (append (firstn i l) (list a)
                  (flet ((franz.nthcdr (ind lis)
                             "equivalent to Franz Lisp 'nthcdr'."
                             (let ((evalind (eval ind)))
                               (if (minusp evalind) (cons nil lis)
                                   (nthcdr evalind lis)))))
                    (franz.nthcdr i l))))))
; dans la liste L il y a peut - etre des () au top niveau : je les vire
; chlorure de vire_nil 
(defun vire_nil (l)
  (and l
       (if (null (car l)) (vire_nil (cdr l))
           (cons (car l) (vire_nil (cdr l))))))
;ne garde que les e'le'ments de L 2 a' 2 distincts au top niveau
(defun un_de_chaque (l)
  (and l
       (if (member (car l) (cdr l) :test #'equal)
           (un_de_chaque (cdr l))
           (cons (car l) (un_de_chaque (cdr l))))))
;retourne la liste de toutes les permutations de L (voir ex plus bas )
(defun permut (l)
  (let ((i 0) (reponse nil) (relais nil))
    (cond
      ((<= (list-length l) 1)
       (list l))
      (t
       (setq relais (permut (cdr l)) a (car l))
       (do ((i i
	       (1+ i)))
	   ((eql i (list-length l)) (un_de_chaque (vire_nil reponse)))
	 (setq reponse
	       (append reponse
		       (append (mapcar #'(lambda (z)
                                           (insertion a z i))
				       relais)))))))))
; ex : ( permut '(1 1 2 2 3 3)) donne la liste des 90 positions concerne'es






