;;  Copyright 2004 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;; Innocent looking problems, such as matrixexp(matrix([x,1,0],[1,1,1],[0,1,1])),
;; generate huge (and most likely worthless) expressions. These huge 
;; expressions strain Maxima's rational function code; to avoid errors
;; such as "..quotient by polynomial of higher degree" I found it necessary to
;;
;;  (1) set gcd == spmod and algebraic == true,

;;  (2) always give ratsimp and fullratsimp a value (even if nil) for its
;;      optional second argument,

;;  (3) set ratvars to an empty list at the start of most functions.

;; I didn't try to find the real cause for these bugs.  The function
;; spectral_rep does check its output. Although these checks are slow,
;; I recommend that they stay.
  
;; Map the CL function f over the elements of a Maxima matrix mat and
;; return a Maxima matrix. The first argument should be a CL function.

($put '$matrixexp 1 '$version)

;; When mat is a square matrix, return exp(mat * x). The second 
;; argument is optional and it defaults to 1.

(defun $matrixexp (mat &optional (x 1))
  (let ((sp) (d) (p) (id) (n ($length ($args mat))) (f))
    ($ratvars)
    ($require_square_matrix mat "$first" "$matrixexp")
    (setq mat ($spectral_rep mat))
    (setq sp ($first mat))
    (setq p ($second mat))
    (setq sp (cons '(mlist simp) 
		   (mapcar #'(lambda (s) ($exp (mult s x))) (cdr sp))))
    (setq d (mult x ($third mat)))
    (setq id ($ident n))
    (setq f id)
    (setq n (+ n 1))
    (dotimes (i n)
      (setq f (add id (div (ncmul2 d f) (- n i)))))
    ($fullratsimp (ncmul2 (ncmul2 sp p) f) nil)))

;; Let f(var) = expr.  This function returns f(mat), where 'mat' is a 
;; square matrix.  Here expr is an expression---it isn't a function!

(defun require-lambda (e n pos fun-name)
  (let ((var))
    (if (and (consp e) (consp (car e)) (eq 'lambda (mop e)) (= 3 (length e))
	     ($listp (nth 1 e)) (setq var (cdr (nth 1 e))) (= n (length var))
	     (every #'(lambda (s) (or (symbolp s) ($subvarp s))) var))
	(list var (nth 2 e))
      (merror "The ~:M argument to `~:M' must be a lambda form with ~:M variable(s)" pos fun-name n))))

(defun $matrixfun (lamexpr mat)
  (let ((z (gensym)) (expr) (var) (sp) (d) (p) (di) 
	(n ($length ($args mat))) (f 0))

    ($require_square_matrix mat "$second" "$matrixexp")
    (setq expr (require-lambda lamexpr 1 "$first" "$matrixfun"))
    (setq var (nth 0 (nth 0 expr)))
    (setq expr (nth 1 expr))
    ($ratvars)
    (setq expr ($substitute z var expr))
    (setq mat ($spectral_rep mat))
    (setq sp ($first mat))  
    (setq p ($second mat))  
    (setq d ($third mat))
    (setq di ($ident n))
    (setq sp (cdr sp))
    (dotimes (i (+ n 1))
      (setq f (add 
	       f
	       (ncmul2 di (ncmul2 
			   (cons '(mlist simp) 
				 (mapcar #'(lambda (s) 
					     ($substitute s z expr)) sp)) p))))
      (setq di (ncmul2 di d))
      (setq expr (div ($diff expr z) (factorial (+ i 1))))) 
    ($fullratsimp (simplify f) nil)))
     
;; Return the residue of the rational expression e with respect to the
;; variable var at the point pt.  Assumptions:

;;  (1) the denominator of e divides ker,
;;  (2) e is a rational expression,
;;  (3) ker is a polynomial,
;;  (4) pt is z zero of ker and ord is its order.

(defun rational-residue (e var pt ker ord)
  (let (($gcd '$spmod) ($algebraic t) ($ratfac nil) (p) (q) (f (sub var pt)))
    ($ratvars)
    (setq e ($fullratsimp e var))
    (setq p ($num e))
    (setq q ($denom e))
    (setq p (mult p ($quotient ker q var)))
    (setq e ($fullratsimp (div p ($quotient ker (power f ord) var)) var))
    ($fullratsimp
     ($substitute pt var (div ($diff e var (- ord 1)) (factorial (- ord 1))))
     nil)))


(defun $spectral_rep (mat)
  ($require_square_matrix mat "$first" "$spectral_rep")
  (let (($gcd '$spmod) ($algebraic t) ($resultant '$subres) (ord) (zi)
	($ratfac nil) (z (gensym)) (res) (m) (n ($length ($args mat))) 
	(p) (p1) (p2) (sp) (proj))

    ($ratvars)
    (setq p ($newdet (sub mat (mult z ($ident n)))))
    (if (oddp n) (setq p (mult -1 p)))

    ;; p1 = (z - z1)(z - z2) ... (z - zk), where z1 thru zk are
    ;; the distinct zeros of p.

    (setq p1 ($first ($divide p ($gcd p ($diff p z) z) z)))
    (setq p2 ($resultant p1 ($diff p1 z) z))

    (cond ((and (not ($constantp p2)) (not (like 0 p2)))
	   ($ratvars)
	   (setq p2 ($sqfr p2))
	   (if (mminusp p2) (setq p2 (mult -1 p2)))
	   (setq p2 (if (mtimesp p2) (margs p2) (list p2)))
	   (setq p2 (mapcar #'(lambda (s) (if (mexptp s) (nth 1 s) s)) p2))
	   (setq p2 `((mtimes simp) ,@p2))
	(mtell "Proviso: assuming ~:M" `((mnotequal simp) ,p2 0))))

    (setq sp ($solve p z))
    (setq sp (mapcar '$rhs (cdr sp)))
    (cond ((not (eq n (apply #'+ (cdr $multiplicities))))
	   (print `(ratvars = ,$ratvars gcd = '$gcd algebraic = ,$algebraic))
	   (print `(ratfac = ,$ratfac))
	   (merror "Unable to find the spectrum")))
   
    (setq res ($fullratsimp (ncpower (sub (mult z ($ident n)) mat) -1) z))
    (setq m (length sp))
    (dotimes (i m)
      (setq zi (nth i sp))
      (setq ord (nth (+ i 1) $multiplicities))
      (push (matrix-map #'(lambda (e) (rational-residue e z zi p ord)) res) 
	    proj))

    (setq proj (nreverse proj))
    (setq m (length proj))
    (dotimes (i m)
      (setq mat (sub mat (mult (nth i sp) (nth i proj)))))
       
    (cond ((check-spectral-rep proj n)
	   (push `(mlist simp) proj)
	   (push `(mlist simp) sp)
	   `((mlist simp) ,sp ,proj, ($fullratsimp mat nil)))
	  (t
	   (merror "Unable to find the spectral representation")))))
  
(defun check-spectral-rep (proj n)
  (let* ((m (length proj)) (okay) (zip ($zeromatrix n n)) (qi) 
	 ($gcd '$spmod) ($algebraic t) ($ratfac nil))
    ($ratvars)

    (setq proj (mapcar #'(lambda (s) ($fullratsimp s nil)) proj))
    (setq okay (like ($ident n) ($fullratsimp (apply 'add proj) nil)))
    
    (dotimes (i m)
      (setq qi (nth i proj))
      (setq qi ($fullratsimp (sub qi (ncmul2 qi qi)) nil))
      (setq okay (and okay (like zip qi))))
    
    (dotimes (i m)
      (setq qi (nth i proj))
      (dotimes (j i)
	(setq okay (and okay (like zip ($fullratsimp
					(ncmul2 qi (nth j proj)) nil))))
	(setq okay (and okay (like zip ($fullratsimp 
					(ncmul2 (nth j proj) qi) nil))))))
    okay))  
      
      

    
    


    
	
