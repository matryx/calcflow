;; Author Barton Willis
;; University of Nebraska at Kearney
;; Copyright (C) 2004, Barton Willis

;; Brief Description: Maxima code for linear homogeneous second order
;; differential equations.

;; Maxima odelin is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License,
;; http://www.gnu.org/copyleft/gpl.html.

;; Maxima odelin has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$lazysolver 1 '$version)

(eval-when
    #+gcl (load compile eval)
    #-gcl (:load-toplevel :compile-toplevel :execute)
    ($load "odeutils"))

(defmvar $aalgsys_is_loquacious nil)

(defun variablep (e)
  (or ($symbolp e) ($subvarp e)))

;; Check that sol is a solution to eqs and that nz is nonvanishing.

(defun $checksolution (sol eqs &optional (nz `((mlist))))
  (let (($gcd '$spmod) ($algebraic t) ($ratfac nil) ($ratprint nil))
    (if (not ($listp sol)) (setq sol `((mlist) ,sol)))
    (if (not ($listp eqs)) (setq eqs `((mlist) ,eqs)))
    (if (not ($listp nz)) (setq nz `((mlist) ,nz)))
    (setq eqs (mapcar #'meqhk (cdr eqs)))
    (setq eqs `((mlist) ,@eqs))
    (and 
     (every #'mequalp (cdr sol))
     (every #'(lambda (s) (variablep ($lhs s))) (cdr sol))
     (every #'(lambda (s) (like 0 s)) (cdr (sratsimp ($substitute sol eqs))))
     (every #'(lambda (s) (not (like 0 s)))
	    (cdr (sratsimp ($substitute sol nz)))))))

;; The function 'checkedalgsys' tries to return the "simplest" solution.
;; it uses this simple-minded measure of simple.

(defun my-expr-size (e)
  (xmy-expr-size ($totaldisrep e)))

(defun xmy-expr-size (e)
  (if (consp e) (apply #'+ (mapcar #'my-expr-size (margs e))) 1))

(defun my-freeof (unks q)
  (every #'(lambda (x) ($freeof x q)) (cdr unks)))

;; This function solves 'eqs' for 'vars' and returns a solution such that
;; no expression in the Maxima list 'nz' vanishes. Since
;; 'algsys' sometimes returns bogus solutions--checkedalgsys checks
;; the putative solutions and rejects bogus solutions.  To return
;; the 'simpliest' solution, we sort the putative solutions using
;; 'my-expr-size.  

(defun $checkedalgsys (eqs vars &optional (nz `((mlist))))
  
  (let ((sol) ($ratfac nil) ($ratprint nil) ($realonly nil) ($algexact t) 
	($gcd '$spmod) ($algebraic t) 
	($programmode t) ($globalsolve nil) ($solveexplicit t) 
	($listconstvars t) ($solveradcan nil) ($ratvars nil))

    (if $aalgsys_is_loquacious
	(mtell "...solving ~:M equations in ~:M variables~%" 
	       ($length eqs) ($length vars)))

    (setq sol (cdr ($algsys eqs vars)))
    (setq sol (sort sol #'(lambda (a b) (< (my-expr-size a) (my-expr-size b)))))
    (dolist (si sol)
      (if ($checksolution si eqs nz) (return si)))))

;; The function 'checkedalgsys' tries to return the "simplest" solution.
;; it uses this simple-minded measure of simple.

(defun nonconstant-factors (e vars)
  (let (acc)
    (setq e ($factor e))
    (setq e (if (mtimesp e) (margs e) (list e)))
    (dolist (ei e `(($set) ,@acc))
      (if (mexptp ei) (setq ei (car (margs ei))))
      (if (not (my-freeof vars ei)) (push ei acc)))))

(defun variablep (e)
  (or ($symbolp e) ($subvarp e)))

(defun $checksolution (sol eqs &optional (nz `((mlist))))
  (let (($gcd '$spmod) ($algebraic t) ($ratfac nil) ($ratprint nil))
    (if (not ($listp sol)) (setq sol `((mlist) ,sol)))
    (if (not ($listp eqs)) (setq eqs `((mlist) ,eqs)))
    (if (not ($listp nz)) (setq nz `((mlist) ,nz)))
    (setq eqs (mapcar #'meqhk (cdr eqs)))
    (setq eqs `((mlist) ,@eqs))
    (and 
     (every #'mequalp (cdr sol))
     (every #'(lambda (s) (variablep ($lhs s))) (cdr sol))
     (every #'(lambda (s) (like 0 s)) (cdr (sratsimp ($substitute sol eqs))))
     (every #'(lambda (s) (not (like 0 s)))
	    (cdr (sratsimp ($substitute sol nz)))))))

(defun unks-in-eq (eq unks)
  (let (($listconstvars nil))
    ($intersection ($setify ($listofvars eq)) unks)))

(defun $aalgsys (e-eqs eqs unks &optional (nz `((mlist))))
  (let ((e-vars) (sol) (e-sol) ($gcd '$spmod) ($algebraic t) 
	($ratvars nil) ($radexpand nil) ($ratfac nil) ($ratprint nil))
    
    (setq unks ($setify unks))
    (setq unks (mbag-map #'$ratdisrep unks))

    (setq eqs ($setify eqs))
    (setq eqs (mbag-map #'$ratdisrep eqs))

    (setq e-eqs ($setify e-eqs))
    (setq e-eqs (mbag-map #'$ratdisrep e-eqs))
    (setq e-eqs (mbag-map #'$factor e-eqs))

    (setq e-eqs ($union e-eqs 
			($subset eqs #'(lambda (w) 
					 (= 1 (number-of-unks w unks))))))

    (setq e-vars (unks-in-eq e-eqs unks))
    (setq e-eqs ($union e-eqs 
			($subset eqs #'(lambda (w) 
					 (like (unks-in-eq w unks) e-vars)))))

    (setq e-eqs (mbag-map #'$ratdisrep e-eqs))
    (setq e-eqs (mbag-map #'$factor e-eqs))
    (setq e-eqs ($disjoin 0 e-eqs))
        
   ;; (displa `((mequal) eeqs ,e-eqs))
    (setq e-eqs ($listify e-eqs))
    (setq e-vars ($listify e-vars))
    (setq eqs ($listify eqs))
    (setq unks ($listify unks))
    
    (block bailout
      (cond ((not ($emptyp e-vars))
	     (setq e-sol ($algsys e-eqs e-vars))
	     (setq e-sol ($setify e-sol))
	     (setq e-sol 
		   ($subset e-sol #'(lambda (w) ($checksolution w e-eqs nz))))
	     (setq e-sol
		   ($subset e-sol #'(lambda (w) (my-freeof $%rnum_list w))))
	     (setq e-sol ($listify e-sol))
	     
	     (cond ((not ($emptyp e-sol))
		    (setq e-sol (margs e-sol))
		    (dolist (ei e-sol)
		     ;; (displa `((mequal) auxeq ,ei))
		      (setq sol ($checkedalgsys 
				 ($append ei ($substitute ei eqs)) unks nz))
		      (if ($listp sol) (return-from bailout sol))))
		   (t ($checkedalgsys ($append e-eqs eqs) unks nz))))
	    (t (return-from bailout ($checkedalgsys eqs unks nz)))))))
		    

      
    
    
	
	

    
    
  
  
  

  
    
    




 
