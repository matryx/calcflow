;; Flatten 
;; Barton Willis
;; University of Nebraska at Kearney (aka UNK)
;;    1 Nov 2002
 
;; License: GPL
;; The user of this code assumes all risk for its use. It has no warranty.
;; If you don't know the meaning of "no warranty," don't use this code. :)

;; Installation and usage:  Put flatten.lisp in a directory that
;; Maxima can find.  (Maxima can find files in directories described
;; in the list file_search_lisp.) To use flatten, begin by loading it.

;; (C1) load("flatten.lisp")$
;; (C2) flatten([x=7,[y+x=0,z+1=0], [[x-y=2]]]);
;; (D2)         [x = 7, y + x = 0, z + 1 = 0, x - y = 2]
;; (C3) m : matrix([a,b],[c,d])$
;; (C4) flatten(args(m));
;; (D4)         [a, b, c, d] 
 
;; Flatten is somewhat difficult to define -- essentially it evaluates an 
;; expression as if its main operator had been declared nary; however, there 
;; is a difference.  We have

;; (C1) load("flatten.lisp");
;; (D1)         flatten.lisp
;; (C2) flatten(f(g(f(f(x)))));
;; (D2)         f(g(f(f(x))))
;; (C3) declare(f,nary);
;; (D3)         DONE
;; (C4) ev(d2);
;; (D4)         f(g(f(x)))
;; (C5) 

;; Unlike declaring the main operator of an expression to be nary, flatten 
;; doesn't recurse into other function arguments.  

;; This is supposed to be a clone of Macsyma's flatten function.  
;; Unlike the Macyma version, this version
;;    (a) handles CRE expressions,
;;    (b) doesn't try to flatten expressions of the form a^(b^c) -- Macsyma's
;;        flatten gives an error about a "wrong number of arguments to "^"."
;;    (c) doesn't try to flatten expressions of the form a=(b=c).

;; There are other functions other than ^ and = that we shouldn't try
;; to flatten -- Bessel functions, etc.  

(in-package :maxima)
($put '$charsets_flatten 1 '$version)

;; Return the operator and argument of the expression e.

(defun get-op-and-arg (e)
  (let ((op) (arg))
    (cond ((or ($atom e) ($subvarp e))
	   (setq op nil)
	   (setq arg nil))
	  ((and (consp (nth 0 e)) ($subvarp (nth 1 e)))
	   (setq op `(,(nth 0 e) ,(nth 1 e)))
	   (setq arg (cddr e)))
	  (t
	   (setq op (nth 0 e))
	   (setq arg (cdr e))))
    (values op arg)))

(defun $charsets_flatten (e)
  (setq e (ratdisrep e))
  (cond ((or ($atom e) ($subvarp e) (or (member ($inpart e 0) (list "^" "=") :test #'equal)))
	 e)
	(t
	 (let ((op (multiple-value-list (get-op-and-arg e))))
	   (setq e (cadr op))
	   (setq op (car op))
	   (setq e (mapcar #'(lambda (x) (flatten-op x op)) e))
	   (setq e (reduce #'append e))
	   (cond ((and (consp (car op)) (eq (caar op) 'mqapply))
		  (append op e))
		 (t
		  `(,op ,@e)))))))
	  
(defun flatten-op (e op)
  (let ((e-op) (e-arg))
    (setq e-op (multiple-value-list (get-op-and-arg e)))
    (setq e-arg (cadr e-op))
    (setq e-op (car e-op))
    (cond ((equal e-op op)
	   (mapcan #'(lambda (x) (flatten-op x op)) e-arg))
	  (t
	   (list e)))))
	   
	   
;;; Cut $every from src/mutils.lisp and paste it here, 
;;; renamed to $charsets_every.
;;; Also rename $flatten to $charsets_flatten.
;;; Robert Dodier 2005/02/22

;;; This function works like the every function in lisp.
;;; It can take a list, or a positive number of arguments returning
;;; true if all its arguments are not false.
;;; Author Dan Stanger 12/1/02
(defmfun $charsets_every (&rest args)
  (let ((n (length args)))
    (cond ((= n 0) (merror "Every must have at least 1 argument"))
	  ((= n 1)
	   (let ((args (first args)))
	     (if (and ($listp args) (> ($length args) 0))
		 (notany #'not (margs args))
		 (if (and ($listp args) (= ($length args) 0)) nil args))))
	  (t (notany #'not args)))))
