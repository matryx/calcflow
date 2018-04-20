;; Fichier lecteur.lsp

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
(macsyma-module lecteur)

(progn (defvar d) (defvar lvar))
;---------------------------------------------------------------------------
;               LE LECTEUR DANS  k[y1, ... ,yn][x1, ... ,xp]
;                rendant la forme distribuee du polynome pol
; constante = [constante, 0, ...,0] avec p exposants nuls
;---------------------------------------------------------------------------
(defun $distri_lect ($pol $lvar)
   (if (meval (list '($is) (list '(mequal) $pol 0))) (cons '(mlist)  nil)
  (cons '(mlist)
        (mapcar #'(lambda (mon) (cons '(mlist) mon))
                (distri_lect $pol (cdr $lvar))))))

; lvar =(x1 x2 .... xp) les yi inconnus
; on prend uniquement le cdr pour trier car le car est le coefficient
; on ordonne dans l'ordre lexicographique decroissant.

(defun distri_lect ($pol lvar)
  (somme_coef 
      (sort (exposants ($expand $pol) lvar) #'lex_mon :key #'cdr)))

(defun somme_coef (pol_dist)
  (and pol_dist
       (somme_coef2 (caar pol_dist) ; le coefficient initial
                    (cdar pol_dist) ; le monome initial
                    pol_dist))
       pol_dist)

(defun somme_coef2 (c m pol_dist) 
  (cond
    ((null (cdr pol_dist)) (rplaca (car pol_dist) c))
    (t (let ((c2 (caar (cdr pol_dist))) (m2 (cdar (cdr pol_dist))))
         (cond
           ((equal m m2) 
            (somme_coef2 ($add_sym c c2) m
                (rplacd pol_dist (cddr pol_dist))))
           (t (rplaca (car pol_dist) c)
              (somme_coef2 c2 m2 (cdr pol_dist))))))))

(defun exposants (pol lvar)
  (if (and (listp pol) (equal 'mplus (caar pol)))
      (mapcar 'expomon (cdr pol)) (list (expomon pol))))
;---------------------------------------------------------------------------
; lecture d'un mono^me :
; Soit un mono^me dans k[x1,...,xn] ou` k est e'ventuellement un anneau
; de polyno^mes sur un corps. On construit une plist 'var_coe :
; si c'est un e'le'ment du corps on le met a l'indicateur : coe
; si c'est un variable on met l'exposant avec comme indicateur la variable.
; Ensuite cre'e' la liste des valeurs lie'es aux variables xi dans la pliste
; et on fait le produit des autres valeurs de cette plist.
; Si on a une constante C sur k on la represente par [C,0,0,...,0] (n ze'ros).
;----------------------------------------------------------------------------
(defun expomon (mon)
  (cond
    ((numberp mon)		       ; on a une cste de k uniquement
     (and (not (zerop mon)) (cons mon (make-list (length lvar)
						 :initial-element 0))))
    (t
     (cond
       ((and (listp mon) (equal 'mtimes (caar mon)))
	(cond
	  ((not (or (and (listp (cadr mon))
			 (equal 'mexpt (caar (cadr mon))))
		    (member (cadr mon) lvar :test #'equal)))
	   ;; le coefficient, eventuellement rationnel, est different de 1
	   (mapc 'lvarexpo (cddr mon))
	   (setf (get 'var_expo 'coe) (cadr mon)))
	  (t
	   ;; le coefficient est e'gal a 1
	   (mapc 'lvarexpo (cdr mon)) 
	   (setf (get 'var_expo 'coe) 1))))
       ;; on a ((mexpt) x 4) ou x:
       (t (lvarexpo mon) (setf (get 'var_expo 'coe) 1)))
     ;; maintenant toutes les donnees sont dans la plist
     ;; reste a bien recoller les morceaux
     (let ((ncoe (cadr (flet ((franz.remprop
                                  (sym indic &aux
                                       (result
                                        (third
                                         (multiple-value-list
                                          (get-properties
                                           (symbol-plist sym)
                                           (list indic))))))
				"equivalent to Franz Lisp 'remprop'."
				(remprop sym indic) result))
                         (franz.remprop 'var_expo 'coe))))
	   (exposant (expomon2 lvar)))
       ;; on n'a retire que les exposants des xi et le coefficient
       ;; numerique de la plist, reste les yi et leur exposants
       ;; a remettre en coefficients.
       (cons (recupcoef (symbol-plist 'var_expo) ncoe) exposant)))))

(defun recupcoef (plist coef)
  (if (null plist) coef
      (let ((yi (car plist)))
        (recupcoef (cddr plist)
            ($mult_sym
                ($exp_sym yi
                    (cadr (flet ((franz.remprop
                                    (sym indic &aux
                                     (result
                                      (third
                                       (multiple-value-list
                                        (get-properties
                                         (symbol-plist sym) (list indic))))))
                                    "equivalent to Franz Lisp 'remprop'."
                                    (remprop sym indic) result))
                           (franz.remprop 'var_expo yi))))
                coef)))))

; surement inutile desormais
(defun makelist (nb list)
     (mapcar #'(lambda (x) nb) list))

; Representation MACSYMA, mmon, de x**i : 
;  x            si i=1 
; ((mexpt) x i) sinon
; on veut recuperer (x i) et mettre la valeur i pour l'indicateur x
; dans la plist var_expo.

(defun lvarexpo (mmon)
  (if (atom mmon) (setf (get 'var_expo mmon) 1)
      (setf (get 'var_expo (cadr mmon)) (caddr mmon))))
; recuperation de la liste des exposants associee aux variables de lvar :

(defun expomon2 (lvar)
  (mapcar #'(lambda (var)
             (chercheexpo
                (cdr (flet ((franz.remprop
                            (sym indic &aux
                                 (result (third
                                          (multiple-value-list
                                           (get-properties
                                            (symbol-plist sym)
                                            (list indic))))))
                            "equivalent to Franz Lisp 'remprop'."
                            (remprop sym indic) result))
                   (franz.remprop 'var_expo var)))))
          lvar))

(defun chercheexpo (expo) (if (null expo) 0 (car expo)))
; en lelisp il faudrait prendre expo
