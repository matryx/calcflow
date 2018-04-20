
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some utilities for working with vectors.

;; Copyright (C)  Nov. 2008  Volker van Nek

;; modified 08-12-03: vector_factor factors lists and matrices
;;          08-12-05: vector_eval: $ratprint set to false
;;          08-12-10: rename stardisp to stardisp1, assign property of $stardisp
;;          08-12-14: vector_eval: cut out sratsimp, rename it to vector_simp
;;                    $vector_rebuild: evaluate mnctimes, bugfix case mdefine

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions at Maxima level:
;; vector_rebuild(x), vector_rebuild(x,[param_list])
;; vector_simp(x),    has evfun property
;; vector_factor(x),  has evfun property
;; extract_equations(x)

;; Operators at Maxima level:
;; |x| vector length
;; x~y cross product (lbp=134, rbp=133)

;; Option variable at Maxima level:
;; vector_factor_minus

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mtimesq

;; In combination with setting listarith & friends to false 
;; mtimesq allows to suppress automatic arithmetrics
;; and to bypass the displa function dim-mquotient:
;;                  1                                       1 [ 2 ]
;;   1/r*[2,3]; --> - [2, 3]   or   1/r*matrix([2],[3]);--> - [   ]
;;                  r                                       r [ 3 ]
;; mtimesq display symbol is "*" and is settable by stardisp.
;; One evaluation steps from mtimesq to mtimes and vice versa.

(meval `(($infix) mtimesq 120 119))
(defprop mtimesq simp-mtimesq operators)

(defun simp-mtimesq (a tmp z)
  (declare (ignore tmp))
  `((mtimesq simp) ,(simplifya (cadr a) z) ,(simplifya (caddr a) z)))

(defmfun mtimesq (a b) `((mtimes simp) ,a ,b))

(putprop 'mtimesq (get 'mtimes 'dissym) 'dissym)

;; extend stardisp in displa.lisp :
(defun stardisp1 (symbol val)
  (declare (ignore symbol))
  (putprop 'mtimes (if val '(#\*) '(#\space)) 'dissym)
  (putprop 'mtimesq (get 'mtimes 'dissym) 'dissym) )
;;
(defprop $stardisp stardisp1 assign)

(defun mtimesqp (expr)
  (and (not (atom expr)) (eq 'mtimesq (caar expr))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; $vector_simp

(defun $vector_simp (expre$$ion)
  (let (($listarith t) ($doallmxops t))
    ($expand (meval `(($ev) ,expre$$ion $infeval))) ))
    ;; mtimesq needs an extra evaluation here

(putprop '$vector_simp t 'evfun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; $vector_rebuild

(defun $vector_rebuild (expr &optional (params '((mlist))) )
  (when (or (not ($listp params))
            (some #'(lambda (p) (or ($constantp p) 
                                    (not (or ($symbolp p) ($subvarp p)))))
                  (cdr params)))
    (merror 
      (format nil "The optional second argument to `vector_rebuild'~%~
        must be a list of symbols or subscripted variables.")))  
  (if (or (atom expr) ($scalarp expr))
    expr
    (let ((op (mop expr))
          (args (margs expr)) )
      (if (not (car args)) (return-from $vector_rebuild expr)) ;; noarg-op
      (cond
        ((eq op 'mequal)
          (cons `(mequal simp)
            (mapcar #'(lambda (arg) ($vector_rebuild arg params)) args) ))
        ((eq op 'mdefine)
          (meval `((mdefine simp) 
            ,(cadr expr) 
            ,($vector_rebuild (caddr expr) params)) ))
        (t
          (vector-rebuild expr (cdr params)) )))))

(defun vector-rebuild (expr params)
  (let (coef-matrix args col-flag tmp-vec
        (res '((mplus) 0))
        (n 0) 
        (nr-pars (length params))
        (vec ($vector_simp expr)) )
    (when (not (vector-p vec)) (return-from vector-rebuild expr))
    (when (zero-vector-p vec) (return-from vector-rebuild vec))
    (when (column-vector-p vec) (setq col-flag t))
    (dolist (a (cdr vec))
      (when col-flag (setq a (cadr a)))
      (setq coef-matrix 
        (append coef-matrix (list (coef-list a params)))) )
    (setq params (append params '(1)))
    (dolist (p params)
      (setq col (mapcar #'(lambda (row) (nth n row)) coef-matrix))
      (incf n)
      (when (some #'(lambda (e) (not (zerop1 e))) col)
        (progn
          (setq tmp-vec `((mlist simp) ,@col) )
          (when col-flag 
            (setq tmp-vec (row-to-column tmp-vec)))
          (setq res 
            (append res `(((mtimes) ,p ,tmp-vec)) ))) ))      
    (meval res) ))

;; e.g. expr=4-2*t+3*s, params=(s t) --> (3 -2 4)
(defun coef-list (expr params)
  (let (res c)
    (dolist (p params)
      (setq c (meval `(($coeff) ,expr ,p)))
      (push c res)
      (setq expr 
        (meval `((mplus) ,expr ((mtimes) ((mminus) ,c) ,p))) ))
    (reverse (cons expr res)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; $extract_equations

(defun $extract_equations (equation)
  (let ((err-msg "Argument to `extract_equations' is not a vector equation.")
        args left right)
    (and (not (atom equation)) 
         (not (eq (mop equation) 'mequal))
         (merror err-msg))
    (setq args (cdr equation))
    (setq left ($vector_simp (car args)))
    (setq right ($vector_simp (cadr args)))
    (cond
      ((and (vector-p left) (vector-p right))) ;; OK
      ;; due to a bug in Maxima allow left or right to be zero:
      ((and (eq 0 left) (vector-p right))
        (setq left 
          (cons '(mlist simp) 
            (make-list (vector-dim right) :initial-element 0))) )
      ((and (eq 0 right) (vector-p left))
        (setq right 
          (cons '(mlist simp) 
            (make-list (vector-dim left) :initial-element 0))) )
      (t
         (merror err-msg) ))
    (when (not ($listp left)) (setq left (column-to-row left)))
    (when (not ($listp right)) (setq right (column-to-row right)))
    (meval `(($map) "=" ,left ,right)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; $vector_factor

(defun $vector_factor (expr)
  (if (atom expr)
    expr
    (let ((op (mop expr)))
      (cond
        ((eq op 'mequal)
          `((mequal simp) ,@(mapcar #'$vector_factor (cdr expr))) )
        ((eq op 'mdefine)
          (meval `((mdefine simp) 
            ,(cadr expr) 
            ,($vector_factor (caddr expr))) ))
        ((member op '(mplus mnctimes crossq) :test #'eq)
          `((,op simp) ,@(mapcar #'$vector_factor (cdr expr))) )
        (t
          (vector-extract-gcd expr) ))))) ;; incl. op=mtimes,mminus

(putprop '$vector_factor t 'evfun)

;; if you whish to factor out a minus sign, set this to true
(defmvar $vector_factor_minus nil)

(defun vector-extract-gcd (expr)
  (let (fac vec args minus-flag $ratprint)
    (setq vec ($vector_simp expr))
    (cond
      ((or (zero-vector-p vec) (zero-$matrix-p vec))
        vec)
      ((or ($listp vec) ($matrixp vec))
        (setq args 
          (if ($listp vec)
            (cdr vec)
            (apply #'append (mapcar #'cdr (cdr vec))) ))
        (setq fac 
          (reduce #'(lambda (a b) ($gcd a b)) (cons (car args) args)))
        (setq vec
          (meval 
            `(($fullmapl) 
              ((lambda) ((mlist) e) (($ratsimp) ((mquotient) e ,fac)))
              ,vec )))
        (and 
          $vector_factor_minus
          (every 
            #'(lambda (a) (eq t (meval `(($is) ((mlessp) ,a 0))))) 
            args)
          (setq minus-flag t)
          (setq vec
            (meval 
              `(($fullmapl) ((lambda) ((mlist) e) ((mminus) e)) ,vec) )))
        (if minus-flag
          (if (eq 1 fac)
            `((mminus) ,vec)
            `((mminus) ((mtimesq) ,fac ,vec)) )
          (if (eq 1 fac)
            vec
            `((mtimesq) ,fac ,vec) )))
      (t
        expr ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vector length

(meval `(($matchfix) $\| $\|))

(defprop $\| simp-vector-length operators)

(defun simp-vector-length (a tmp z)
  (declare (ignore tmp) (ignore z))
  (oneargcheck a)
  (setq a ($vector_simp (cadr a)))
  (cond
    (($scalarp a)
      (meval `((mabs simp) ,a)) )
    ((vector-p a)
      (let (args) 
        (setq args 
          (if ($listp a)
            (cdr a)
            (mapcar #'cadr (cdr a)) ))
        (setq args (mapcar #'(lambda (a) (meval `((mexpt) ,a 2))) args))
        (meval `(($sqrt simp) ((mplus simp) ,@args))) ))
    (t
      `(($\|) ,a) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cross product

(meval `(($infix) $\~ 134 133)) ;; lbp and rbp copied from vect.mac

(defprop $\~ simp-vector-cross operators)

;; if it seems fit we can suppress automatic simplification 
;; by using an alias:
;;
;; (meval `(($infix) crossq 134 133))
;; (putprop 'crossq '(#\~) 'dissym)
;;
;; (defun simp-vector-cross (a tmp z)
;;   (declare (ignore tmp) (ignore z))
;;   (twoargcheck a)
;;   `((crossq simp) ,(cadr a) ,(caddr a)) )
;;    ;; again, here is one more step of evaluation needed

;; (defmfun crossq (a b)
;;   (let ((v ($vector_simp a))
;;         (w ($vector_simp b))
;;         cross col-flag dim-v dim-w) ;;))

(defun simp-vector-cross (a tmp z)
  (declare (ignore tmp) (ignore z))
  (twoargcheck a)
  (let ((v ($vector_simp (cadr a)))
        (w ($vector_simp (caddr a)))
        cross col-flag dim-v dim-w)
    (if (and (vector-p v) (vector-p w))
      (progn
        (when (column-vector-p v) 
          (setq v (column-to-row v) col-flag t))
        (when (column-vector-p w) 
          (setq w (column-to-row w) col-flag t))
        (setq dim-v (1- (length v)) dim-w (1- (length w)))
        (cond
          ((and (= 2 dim-v) (= 2 dim-w))
            ;; v[1]*w[2]-v[2]*w[1]
            (meval `((mplus simp)
              ((mtimes simp) ,(cadr v) ,(caddr w))
              ((mminus) ((mtimes simp) ,(caddr v) ,(cadr w))) )))
          ((and (= 3 dim-v) (= 3 dim-w))
            ;; [ v[2]*w[3]-v[3]*w[2],
            ;;   v[3]*w[1]-v[1]*w[3],
            ;;   v[1]*w[2]-v[2]*w[1] ]
            (setq cross
              `((mlist simp)
                ,(meval `((mplus simp)
                  ((mtimes simp) ,(caddr v) ,(cadddr w))
                  ((mminus) ((mtimes simp) ,(cadddr v) ,(caddr w))) ))
                ,(meval `((mplus simp)
                  ((mtimes simp) ,(cadddr v) ,(cadr w))
                  ((mminus) ((mtimes simp) ,(cadr v) ,(cadddr w))) ))
                ,(meval `((mplus simp)
                  ((mtimes simp) ,(cadr v) ,(caddr w))
                  ((mminus) ((mtimes simp) ,(caddr v) ,(cadr w))) )) ))
            (if col-flag 
              (row-to-column cross)
              cross ))
          (t
            `(($\~) ,v ,w) ) ))
      `(($\~) ,v ,w) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some small helpers

(defun column-to-row (col) ;; assume col to be a column vector (a $matrix)
  (cons '(mlist simp) 
    (mapcar #'cadr (cdr col)) ))

(defun row-to-column (row) ;; assume row to be a row vector (a mlist)
  (cons '($matrix simp)
    (mapcar #'(lambda (e) (list '(mlist simp) e)) 
      (cdr row) )))

(defun vector-dim (vec) ;; assume vec to be a row or column vector
  (length (cdr vec)) )

(defun column-vector-p (obj)
  (and ($matrixp obj)
       (= 2 (length (cadr obj))) ))

(defun vector-p (obj)
  (or ($listp obj) (column-vector-p obj)))

(defun zero-vector-p (obj)
  (or (zero-mlist-p obj)
      (and (zero-$matrix-p obj) (= 2 (length (cadr obj)))) ))

(defun zero-mlist-p (obj)
  (and ($listp obj) (every #'zerop1 (cdr obj)) ))

(defun zero-$matrix-p (obj)
  (and ($matrixp obj) (every #'zero-mlist-p (cdr obj)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
