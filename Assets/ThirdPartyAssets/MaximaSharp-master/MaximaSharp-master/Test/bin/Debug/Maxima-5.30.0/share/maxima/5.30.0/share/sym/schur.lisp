; schur.lsp

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

;----------------------------------------------------------------------------
;                 FONCTIONS DE SCHUR et CHANGEMENTS DE BASES
;----------------------------------------------------------------------------
; DIFFERENCE ENTRE COMPILE ET INTERPRETE : voir fonction schur2comp_pol
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;            PASSAGE DES FORMES MONOMIALES AUX FONCTIONS DE SCHUR

(in-package :maxima)
(macsyma-module schur)

(mdefprop $mon2schur
    ((lambda ()) ((mlist) $part)
     ((mprog) (($operation)) (($mon2schur_init) $part)))
    mexpr)
(add2lnc '(($mon2schur) $part) $functions)
(mdefprop $kostka
    ((lambda ()) ((mlist) $part1 $part2)
     ((mprog) (($operation)) (($kostka_init) $part1 $part2)))
    mexpr)
(add2lnc '(($kostka) $part1 $part2) $functions)
(mdefprop $treinat
    ((lambda ()) ((mlist) $part)
     ((mprog) (($operation)) (($treinat_init) $part)))
    mexpr)
(add2lnc '(($treinat) $part) $functions)
;            PASSAGE DES FONCTIONS DE  SCHUR AUX COMPLETES
(mdefprop $schur2comp
    ((lambda ()) ((mlist) $comp $listofvars)
     ((mprog) (($operation)) (($schur2comp_init) $comp $listofvars)))
    mexpr)
(add2lnc '(($schur2comp) $comp $listofvars) $functions)

;========================================================================
;                 PASSAGE DES FONCTIONS DE SCHUR
;                     AUX FONCTIONS COMPLETES
; On se donne un polyno^me en h1, h2, ... repre'sentant un polyno^me
; en les fonctions comple`tes.
; on recupere une liste dont chaque element est une liste dont
; le premier terme est un entier et le reste une partition renversee
; representant la fonction de Schur associ\'ee.
; REP(partition) = [partition](1)
;========================================================================
; l'entree est un polynome en les hi
; l'entree est une liste que l'on ordonne

(defun $schur2comp_init ($comp $listofvars)
  (cond
    ((eql '$pol $schur2comp) (schur2comp_pol $comp (cdr $listofvars)))
    (t (cons '(mlist)
             (schur2comp
                 (ch2rep (sort (cdr $comp) '>)))))))
;.........................................................................
; Si la donnee est un polynome en les fonctions completes.
; schur2comp rend '(mlist) en tete de chaque terme partitionne
; le coefficient est donc en cadr

(defun schur2comp_pol ($pol listofvars)
  (do ((polpart (pol2part $pol listofvars) (cdr polpart)) (sol 0))
      ((null polpart) sol)
    (setq sol
          ($add_sym
              ($fadd_sym 
                        (mapcar #'(lambda (l) 
                                     (let ((coef (caar polpart)))
                                           ($fmult_sym
                                                (list (cadr l) coef
                                                (cons '($S array)
			    			      (cddr l))))))
                                 (schur2comp (cdar polpart))))
              sol))))
; l'egalite nous importe pas.
(defun lexinv_type1 (terme1 terme2)
  (2lexinv_type1 (cddr terme1) (cddr terme2)))

(defun 2lexinv_type1 (1part 2part)
  (cond
    ((null (car 1part)) nil)
    ((null (car 2part)) t)
    ((< (car 1part) (car 2part))
     nil)
    ((> (car 1part) (car 2part))
     t)
    (t (2lexinv_type1 (cdr 1part) (cdr 2part)))))
;........................................................................
; pol2part permet a` partir d'un polynome en les monomes
; h^a = h1^a1 ... hn^an
; de recuperer la partition [1,a1,2,a2,...,n,an] sous type 2 (cf. structure)
; en representation creuse ,i.e. si hi=0 on ne retrouve par le couple (i,ai).
;........................................................................
(defun pol2part ($pol listofvar)
  (let ((i 0) (varetdegre (chvaretdegre listofvar)))
    (mapcar #'(lambda (l)
               (setq i (1+ (cdr varetdegre)))
               (cons (car l)
                     (mapcan #'(lambda (nb)
                                (setq i (1- i))
                                (and (not (eq nb 0)) (list i nb)))
                             (nreverse (cdr l)))))
            (lect $pol
                  (cons 'aa (lvar_lettre (cdr varetdegre) nil
                      (flet ((franz.concat (&rest args)
                                 "equivalent to Franz Lisp 'concat'."
                                 (values (intern
                                          (format nil "~{~A~}" args)))))
                        (franz.concat '$ (car varetdegre)))))))))

(defun chvaretdegre (listofvar)
  (let ((hj (cdr (flet ((franz.exploden (arg)
                            "equivalent to Franz Lisp 'exploden'."
                            (map 'list #'char-code
                                 (prin1-to-string arg))))
                   (franz.exploden
                       (car (last (sort listofvar
                                     'string-lessp)))))))
        (i 1))
     (cons (flet ((franz.ascii (charcode)
                     "equivalent to Franz Lisp 'ascii'."
                     (intern (string (code-char charcode)))))
            (franz.ascii (car hj)))
          (apply '+
                 (mapcar #'(lambda (nbascii)
                            (prog1 (* i (- nbascii 48))
                              (setq i (* i 10))))
                         (reverse (cdr hj)))))))
;........................................................................
; si au depart on a :
;REP[part]=[partition](2) au depart
;REP[part]=[partition](1) en sortie mais sous forme :
; ( ((mlist).termpart) ...) qui permet d'utiliser la fonction : somme
; du fichier util.l qui s'attend a trouver la longueur des partition en tete
; de chaque terme partitionne. On le remplace donc par '(mlist)
; qui n'est pas inutile
; [partition](2)

(defun schur2comp (part)
  (let ((part1 (ch1rep part))) 
    (mapcar #'(lambda (2part)
               (cons '(mlist)
                     (cons (kostka 2part part1) (reverse 2part))))
            (mapcar 'ch1rep (treinat part)))))
;========================================================================
;                 PASSAGE DES FORMES MONOMIALES 
;                     AUX FONCTIONS DE SCHUR
; On donne une partition renversee repre'sentant une fonction de Schur
; on recupere un polynome symetrique contracte.
; REP(partition) = [partition](1)
;========================================================================
; dans util.l ==> ($x1 ... $xn)
; pour $fadd_sym

(defun $mon2schur_init ($rpart)
  (let ((part (reverse (cdr $rpart)))
        (lvar (lvar (apply '+ (cdr $rpart)) nil)))
             ($fadd_sym
                 (cons 0
                      (mapcar #'(lambda (2part)
                                 (ecrit_mon 2part lvar (kostka part 2part)))
                              (mapcar 'duale21 (treinat (duale12 part))))))))

; etant donne un partition de representation [partition](1), on
; recupere sa forme duale sous forme [partition](2)
(defun duale12 (partition)
  (let ((ai 0)) 
    (nreverse
        (mapcon #'(lambda (restpart)
                   (setq ai
                         (1+ ai))
                   (cond
                     ((null (cdr restpart))
                      (append restpart (list ai)))
                     (t (let ((mi (- (car restpart)
                                     (cadr restpart))))
                          (and (< 0 mi)
                               (list mi ai))))))
                partition))))
;etant donne un partition de representation [partition](2), on
; recupere sa forme duale sous forme [partition](1)
;(defun duale21 (partition)
;  (let ((m1 (cadr partition)))
; (2duale21 (cddr partition) (list m1)
;          (* m1 (car partition))
;         m1)))
;(defun 2duale21 (part2 part1 p1 p2) 
;  (cond ((null part2) (nconc part1 (make-list (- p1 p2) :initial-element 1)))
;       (t (let ((nxpart (+ (cadr part2) (car part1))))
;         (2duale21 (cddr part2) 
;                  (cons nxpart
;                        part1)
;                      (+ p1 (* (car part2) (cadr part2)))
;                     (+ p2 nxpart))))))
(defun duale21 (partition)
  (let ((lmultiplicites_lparts
            (chmultiplicites_parts partition nil nil)))
    (2duale21 (car lmultiplicites_lparts)
        (cons 0 (cdr lmultiplicites_lparts)) nil)))
(defun 2duale21 (lmulti lpart partition1_duale)
  (cond
    ((null (cdr lmulti))
     (nconc partition1_duale
            (make-list
                (- (cadr lpart) (car lpart))
                 :initial-element  (car lmulti))))
    (t (2duale21 (cdr lmulti) (cdr lpart)
           (nconc partition1_duale
                  (make-list
                        (- (cadr lpart) (car lpart))
                       :initial-element  ($fadd_sym lmulti)))))))

(defun chmultiplicites_parts (partition lmulti lpart)
  (cond
    ((null partition) (cons lmulti lpart))
    (t (chmultiplicites_parts (cddr partition)
           (cons (cadr partition) lmulti) (cons (car partition) lpart)))))

;========================================================================
;                  NOMBRES DE KOSTKA
;            (Algorithme : Philippe ESPERET)
;========================================================================
; REP(partition) = [part](1)

(defun $kostka_init ($1part $2part)
  (kostka (cdr $1part) (cdr $2part)))

(defun kostka (l m)
 (list-length (good_tab0 l (make-list (apply '+ l) :initial-element 0) m)))

(defun firstn (n l)
  (cond
    ((null l) nil)
    ((plusp n)
     (cons (car l)
           (firstn (1- n)
                   (cdr l))))
    (t nil)))
; normalement cette fonction existe en Common  sous le nom de "last"
(defun lastn (l n)
   (nreverse (firstn n (reverse l))))

(defun good_tab0 (l lcont ltas)
  (let ((l1 nil) (rep nil) (relais nil))
       (cond
	 ((eql 1 (list-length l))
	  (mapcar 'list (good_line (car l) lcont ltas)))
	 (t
	  (setq l1 (good_line (car l) lcont ltas))
	  ;; (print "tete des tableaux possibles " L1)
	  (do nil 
	      ((null l1))
	    (setq relais
		  (good_tab0 (cdr l) (car l1) (new_tas0 (car  l1) ltas)))
	    ;; (print " car L1 future tete "(car L1) " et relais "relais) 
	    (if (not relais) (setq l1 (cdr l1))
		(setq rep (nconc rep (insert_tete (car l1) relais)) 
		      l1 (cdr l1))))
	  rep))))

;L liste de listes : retourne la meme liste ou les listes ont ete modifiees
; par insertion de i en tete
(defun insert_tete (i l)
  (if (null l) (list (list i))
      (append (mapcar #'(lambda (z) (cons i z)) l))))
;ote du tas Ltas les elts de L1,avec les not ci-dessus Ltas=(3 2 1) cad
; 3 "1", 2 "2" et 1 "3"
(defun new_tas0 (l1 ltas)
  (if (null l1) ltas
      (new_tas0 (cdr l1)
          (append (firstn (1- (car l1))
                          ltas)
                  (list (1- (nth (1- (car l1)) ltas)))
                  (lastn ltas (- (list-length ltas) (car l1))
                         )))))
(defun good_line (taille lcontrainte ltas)
  (good_length taille (good_line0 taille lcontrainte ltas)))


(defun good_line0 (taille lcontrainte ltas)
  (let ((i 0) (lotas (list-length ltas)) (avanti nil) (rep nil))
          ; (print "taille = "taille "  Ltas" Ltas "GREP "rep)
       (unless (or (null lcontrainte) (zerop taille))
	 (setq i (1+ (car lcontrainte)))
	 (do nil 
	     ((< lotas i))
	   (if (zerop (nth (1- i) ltas))
	       (setq i (1+ i))
	       (setq rep
		     (append rep
			     (insert_tete 
			      i
			      (good_line0 (1- taille)
					  (cdr lcontrainte)
					  (append
					   (make-list (1- i)
						      :initial-element 0)
					   (list (1- (nth (1- i) ltas)))
					   (lastn ltas (- lotas i))
					   ))))
		     i (1+ i)
		     avanti t)))
	 (if avanti rep nil))))

(defun good_length (taille l)
  (if (null l) nil
      (if (eql taille (list-length (car l)))
          (cons (car l) (good_length taille (cdr l)))
          (good_length taille (cdr l)))))

;=======================================================================
;               TREILLIS DES PARTITIONS DANS L'ORDRE NATUREL
; ETANT DONNE UNE PARTITION I ON RAMENE LES PARTITIONS DE MEME
; POIDS INFERIEURES A I.
;=======================================================================
; REP(partition) = [part](1) en entree et en sortie

(defun $treinat_init ($partition1)
  (cons '(mlist)
        (mapcar #'(lambda (part) (cons '(mlist) (ch1rep  part)))
                (treinat (ch2rep (cdr $partition1))))))
; REP(partition) = [part](2) en entree et en sortie
(defun treinat (part2) (soustreillisnat (list part2) nil))
; prendre a chaque fois la plus petite dans l'ordre lexicographique
(defun soustreillisnat (lpartnt lpartt)
  (cond
    ((null lpartnt) lpartt)
    (t (soustreillisnat
           (union_sym (cdr lpartnt)
                     (sort (crefils_init (car lpartnt)) '$lex))
           (cons (car lpartnt) lpartt)))))
; deux listes identiquement ordonnees par lex
(defun union_sym (l1 l2)
  (cond
    ((null l2) l1)
    ((null l1) l2)
    ((equal (car l1) (car l2)) (union_sym l1 (cdr l2)))
    (($lex (car l1) (car l2)) (union2 l1 l2) l1)
    (t (union2 l2 l1) l2)))
(defun union2 (l1 l2)
  (and l2
       (cond
         ((null (cdr l1)) (rplacd l1 l2))
         (t (let ((part1 (cadr l1)) (part2 (car l2)))
              (cond
                ((equal part1 part2) (union2 l1 (cdr l2)))
                (($lex part1 part2) (union2 (cdr l1) l2))
                (t (setq ll1 (cdr l1))
                   (union2 (cdr (rplacd l1 l2)) ll1))))))))
(defun crefils_init (part) (crefils (reverse part) nil nil))
; part = (an mn ... a2 m2 a1 m1) an > ... > a2 > a1
; debut = (a(i-1) m(i-1) ... a1 m1) 
; rfin = (mi ai m(i+1) a(i+1) ...)
; evite l'identite
(defun crefils (rfin debut lfils)
  (cond
    ((null rfin) lfils)
    (t (let ((ai (cadr rfin)) (mi (car rfin)) (rfin (cddr rfin)))
         (cond
           ((and (null rfin) (eql 1 mi)) lfils)
           (t (crefils rfin (cons ai (cons mi debut))
                       (cons (tombecube rfin ai mi debut) lfils))))))))
; ai --> ai-1 et mi reste si ai > 1
; disparition ai --> ai-1 = 0
; disparition ai --> ai-1 = 0
; ai --> ai + 1 = 2
; il en reste mi-2 egales a 1
(defun tombecube (rfin ai mi debut)
  (cond
    ((eql 1 mi)
     (cond
       ((eql 1 ai) (reverse (arrivecube rfin)))
       (t (nconc (reverse (arrivecube rfin))
                 (met (1- ai)
                      debut)))))
    (t (cond
         ((eql 1 ai)
          (cond
            ((eql 2 mi) (reverse (rmet 2 rfin)))
            (t (reverse (cons (- mi 2)
                              (cons 1 (rmet 2 rfin)))))))
         (t (cond
              ((eql 2 mi)
               (nconc (reverse (rmet (1+ ai)
                                     rfin))
                      (met (1- ai)
                           debut)))
              (t (nconc (reverse (cons (- mi 2)
                                       (cons ai
                                        (rmet
                                         (1+ ai)
                                         rfin))))
                        (met (1- ai)
                             debut)))))))))
; rpart = (m a ...)
; aj = a(i-1) ==> m(i-1) --> m(i-1) +1
(defun rmet (aj rpart)
  (cond
    ((null rpart) (list 1 aj))
    ((eql aj (cadr rpart))
     (cons (1+ (car rpart))
           (cdr rpart)))
    (t (cons 1 (cons aj rpart)))))
; part = part2 sens croissant des parts
(defun met (aj part)
  (cond
    ((null part) (list aj 1))
    ((eql aj (car part))
     (cons aj
           (cons (1+ (cadr part))
                 (cddr part))))
    (t (cons aj (cons 1 part)))))
; part = (mj aj ...) un aj passe a  aj+1
; mj = 1
(defun arrivecube (rpart)
  (cond
    ((eql 1 (car rpart))
     (rmet (1+ (cadr rpart))
           (cddr rpart)))
    (t (cons (1- (car rpart))
             (cons (cadr rpart)
                   (rmet (1+ (cadr rpart))
                         (cddr rpart)))))))






