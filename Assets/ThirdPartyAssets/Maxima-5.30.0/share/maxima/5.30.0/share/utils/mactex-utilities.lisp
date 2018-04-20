
;; Additional mactex utilities.
;; All functions are based on code by Richard J. Fateman, copyright 1987.
;; Fateman's code was ported to Common Lisp by William Schelter.

;; If you want LaTeX style quotients, first load mactex and second
;; define tex-mquotient as follows

(defun tex-mquotient (x l r)
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (tex (cadr x) (append l '("\\frac{")) nil 'mparen 'mparen)
	r (tex (caddr x) (list "}{") (append '("}") r) 'mparen 'mparen))
  (append l r))

;; (defprop mtimes ("\\,") texsym)
;; adds a thin space.
;; The following tells TeX to use whatever it thinks best.
;; (defprop mtimes (" ") texsym)

;; To use amsmath style matrices, define tex-matrix as
;; (to TeX the code, you'll need to \usepackage{amsmath})

(defun tex-matrix (x l r) ;; matrix looks like ((mmatrix)((mlist) a b) ...)
  (append l `("\\begin{pmatrix}")
	 (mapcan #'(lambda(y)
			  (tex-list (cdr y) nil (list " \\\\ ") " & "))
		 (cdr x))
	 '("\\end{pmatrix}") r))

