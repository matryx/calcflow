;; This is mostly from texmacs-maxima-5.9.2.lisp
;;    Small changes to mactex.lisp for interfacing with TeXmacs
;;    Andrey Grozin, 2001-2005

(in-package :maxima)

(declare-top
	 (special lop rop $gcprint $inchar)
	 (*expr tex-lbp tex-rbp))

(defun main-prompt ()
  (format () "(~A~D) "
    (print-invert-case (stripdollar $inchar)) $linenum))

(defun tex-stripdollar (x)
  (let ((s (quote-% (maybe-invert-string-case (symbol-name (stripdollar x))))))
    (if (> (length s) 1)
        (concatenate 'string "\\mathrm{" s "}")
      s)))
;(defun tex-stripdollar (sym)
;  (or (symbolp sym) (return-from tex-stripdollar sym))
;  (let* ((name (quote-% (print-invert-case sym)))
;      (name1 (if (memq (elt name 0) '(#\$ #\&)) (subseq name 1) name))
;      (l (length name1)))
;    (if (eql l 1) name1 (concatenate 'string "\\mathrm{" name1 "}"))))


;; Also, we should output f(x)^2, not f^2(x)

(defun tex-mexpt (x l r)
  (let((nc (eq (caar x) 'mncexpt)))	; true if a^^b rather than a^b
    (setq l (if (and (numberp (cadr x)) (numneedsparen (cadr x)))
                (tex (cadr x) (cons "\\left(" l) '("\\right)") lop (caar x))
		(tex (cadr x) l nil lop (caar x)))
          r (if (mmminusp (setq x (nformat (caddr x))))
		;; the change in base-line makes parens unnecessary
		(if nc
		    (tex (cadr x) '("^ {-\\langle ")(cons "\\rangle }" r) 'mparen 'mparen)
		    (tex (cadr x) '("^ {- ")(cons " }" r) 'mparen 'mparen))
		(if nc
		    (tex x (list "^{\\langle ")(cons "\\rangle}" r) 'mparen 'mparen)
		    (if (and (integerp x) (< x 10))
			(tex x (list "^")(cons "" r) 'mparen 'mparen)
			(tex x (list "^{")(cons "}" r) 'mparen 'mparen)))))
    (append l r)))

;; binomial coefficients

(defun tex-choose (x l r)
  `(,@l
    "\\binom{"
    ,@(tex (cadr x) nil nil 'mparen 'mparen)
    "}{"
    ,@(tex (caddr x) nil nil 'mparen 'mparen)
    "}"
    ,@r))

;; Integrals, sums, products

(defun tex-int (x l r)
  (let ((s1 (tex (cadr x) nil nil 'mparen 'mparen)) ;;integrand delims / & d
	(var (tex (caddr x) nil nil 'mparen rop))) ;; variable
    (cond((= (length x) 3)
	  (append l `("\\int {" ,@s1 "}{\\;d" ,@var "}\\big.") r))
	 (t ;; presumably length 5
	  (let ((low (tex (nth 3 x) nil nil 'mparen 'mparen))
		;; 1st item is 0
		(hi (tex (nth 4 x) nil nil 'mparen 'mparen)))
	    (append l `("\\int_{" ,@low "}^{" ,@hi "}{" ,@s1 "\\;d" ,@var "}\\big.") r))))))

(defun tex-sum(x l r)
  (let ((op (cond ((eq (caar x) '%sum) "\\sum_{")
		  ((eq (caar x) '%product) "\\prod_{")
		  ;; extend here
		  ))
	;; gotta be one of those above
	(s1 (tex (cadr x) nil nil 'mparen rop))	;; summand
	(index ;; "index = lowerlimit"
	 (tex `((mequal simp) ,(caddr x),(cadddr x)) nil nil 'mparen 'mparen))
	(toplim (tex (car(cddddr x)) nil nil 'mparen 'mparen)))
    (append l `( ,op ,@index "}^{" ,@toplim "}{" ,@s1 "}\\big.") r)))

(defun tex-lsum(x l r)
  (let ((op (cond ((eq (caar x) '%lsum) "\\sum_{")
		  ;; extend here
		  ))
	;; gotta be one of those above 
	(s1 (tex (cadr x) nil nil 'mparen rop))	;; summand
	(index ;; "index = lowerlimit"
	 (tex `((min simp) , (caddr x), (cadddr x))  nil nil 'mparen 'mparen)))
    (append l `( ,op ,@index "}}{" ,@s1 "}\\big.") r)))

;; This is a hack for math input of integrals, sums, products

(defmfun $tmint (a b f x) ($integrate f x a b))

(defmspec $tmsum (l) (setq l (cdr l))
  (if (= (length l) 3)
      (dosum (caddr l) (cadar l) (meval (caddar l)) (meval (cadr l)) t)
      (wna-err '$tmsum)))

(defmspec $tmlsum (l) (setq l (cdr l))
  (or (= (length l) 2) (wna-err '$tmlsum))
  (let ((form (cadr l))
        (ind (cadar l))
        (lis (meval (caddar l)))
        (ans 0))
       (or (symbolp ind) (merror "Second argument not a variable ~M" ind))
       (cond (($listp lis)
              (loop for v in (cdr lis)
                    with lind = (cons ind nil)
                    for w = (cons v nil)
                    do
                    (setq ans (add* ans  (mbinding (lind w) (meval form)))))
                   ans)
           (t `((%lsum) ,form ,ind ,lis)))))

(defmspec $tmprod (l) (setq l (cdr l))
  (if (= (length l) 3)
      (dosum (caddr l) (cadar l) (meval (caddar l)) (meval (cadr l)) nil)
      (wna-err '$tmprod)))

(defun tex-mlabel (x l r)
  (tex (caddr x)
    (append l
      (if (cadr x)
	  (list (format nil "(~A) " 
                        (print-invert-case (stripdollar (cadr x)))))
        nil))
    r 'mparen 'mparen))

(defun qndispla (form)
  (let (($display2d nil))
    (displa form)))

(defun latex (x)
  (mapc #'princ
	(if (and (listp x) (cdr x)
		 (equal (string-right-trim '(#\Space) (cadr x)) "Is"))
	   (qndispla x)
	  (tex x '("") '("
") 'mparen 'mparen))))

(let ((old-displa (symbol-function 'displa)))
  (defun displa (form)
    (if (eq $display2d '$emaxima)
        (latex form)
      (funcall old-displa form))))
