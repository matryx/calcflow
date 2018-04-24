(in-package :maxima)

(defun $lexicographique? ($a $b)
   (2lexinv_type1 (cddr $a) (cddr $b)))

(defun $longueur? ($a $b)
   (longueur? (cddr $a) (cddr $b)))

(defun longueur? (tt1 t2)
    (let ((l1 (longueur tt1)) (l2 (longueur t2))) ; longueur est dans util.lsp
         (or (< l1 l2)
             (and (= l1 l2)
                  ($lex tt1 t2)))))

; on a un polynome ordonne' mais dont les monomes sont eventuellement
; egaux. Il s'agit d'additionner les coefficients concerne's.
; le polynome est sous sa representation distribue'e en macsyma :
; ((mlist simp) ((mlist simp) coe e1 e2 ... en) ....)

(defun $simplifie_dist ($polynome)
    (simplifie_dist (cadr $polynome) (cdr $polynome)) 
    $polynome)

(defun simplifie_dist (polynome $monome) 
  (if (null (cddr polynome)) nil
    (let (($mon2 (cadr polynome))) 
      (if (not (equal (cddr $mon2) (cddr $monome)))
	  (simplifie_dist (cdr polynome) $mon2)
	(progn (rplaca (cdr $monome)
		       (+ (cadr $monome) (cadr $mon2)))
	       (rplacd polynome (cddr polynome))
	       (simplifie_dist polynome $monome))))))

(defun $insert ($monome $polynome)
   (insert $monome $polynome)
   $polynome)

(defun insert ($monome polynome)
   (if (null (cdr polynome)) (rplacd polynome (list $monome))
       (let (($mon2 (cadr polynome)))
            (cond ((equal (cddr $mon2) (cddr $monome))
                   (let ((coe (+ (cadr $monome) (cadr $mon2))))
                     (if (= 0 coe)
                         (rplacd polynome (cddr polynome))
                         (rplaca (cdr $mon2) coe))))
                  (($longueur? $monome $mon2)
                   (insert $monome (cdr polynome)))
                  (t (rplacd polynome
                             (cons $monome (cdr polynome))))))))
                  
               
