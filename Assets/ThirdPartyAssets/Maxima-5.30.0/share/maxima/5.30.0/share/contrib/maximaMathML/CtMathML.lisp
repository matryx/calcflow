
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Purpose:  Generate MathML Content code from MAXIMA
;;;  File: CtMathML.lsp
;;;  Author: Paul S. Wang
;;;  Date: March 1999
;;;          (c) copyright 1999  Kent State University
;;;                    all rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Usage:     compile this file with UNIX command
;;                %mc CtMathML.lsp
;;             which produces CtMathML.o
;;
;;            load/and link into MAXIMA by MAXIMA top-level comamnd
;;                loadfile("loadmathml.lsp");
;;
;;            Once loaded,  use the command ctmathml(expr [,file])
;;
;; Author: Paul S. Wang
;; Date: 4/99
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module mathml)
(declaim (special lop rop ccol *row *indent* mPrport))


(defmfun $ctmathml (&rest margs)
         (prog (ccol *row* *indent* filename 
		 mexpress mPrport x y lop rop)
	   (setq lop 'mparen rop 'mparen)
           (setq mexpress (car margs))
           (setq ccol 1 *indent* 0 *row* t)
           (cond
             ((null mexpress) (princ " NO EXPRESSION GIVEN ")
              (return nil))
             ((null (cdr margs)) (setq filename nil) (setq mPrport t))
             ((null (cddr margs)) (setq filename (cadr margs))
              (setq mPrport
                    (open (fullstrip1 (cadr margs))
                        :direction :output 
                        :if-exists :append
                        :if-does-not-exist :create)))
             (t (princ " wrong No. of Arguments given ")))
           (when (symbolp (setq x mexpress))
             (setq x ($verbify x))
             (cond
               ((setq y (mget x 'mexprer))
                (setq mexpress
                      (list '(mdefine) (cons (list x) (cdadr y))
                            (caddr y))))
               ((setq y (mget x 'mmacro))
                (setq mexpress
                      (list '(mdefmacro) (cons (list x) (cdadr y))
                            (caddr y))))
               ((setq y (mget x 'aexpr))
                (setq mexpress
                      (list '(mdefine)
                            (cons (list x 'array)
                                  (cdadr y))
                            (caddr y)))))
	   ) ;; end of when
           (when (and (consp mexpress) (consp (car mexpress))
                      (eq 'mlabel (caar mexpress)))
                (setq mexpress (cadr mexpress))
	   )
           (tprinc "<math  xmlns='http://www.w3.org/1998/Math/MathML'>")
           (ctmathml (nformat mexpress))  ;;; call engine
	   (tprinc "</math>")
           (when filename (terpri mPrport) (close mPrport))
           (return 'done)))

(defun masymbol(op l)
(let ((ans (assoc op  l))) 
      (if ans (cdr ans) nil)
))

(defun mpsymbol(op l)
   (cond  ((null l) nil)
          ((eq op (cdar l)) (caar l))
          (t (mpsymbol op (cdr l)))
  )
)

(defun ctmathml(exp)
  (cond ((atom exp) (a2ml exp))     ;; atoms
        ((fractionp exp) nil)       ;; fractional number
	((get (caar exp) 'ctfun)    ;; known function
	    (op2ml (caar exp) (cdr exp)))
	((member 'array (car exp) :test #'eq)
		    (ctarray exp))
        ((get (caar exp) 'ct-proc)
             (funcall (get (caar exp) 'ct-proc) (caar exp)  (cdr exp))
	)
	((cpxp exp) nil)      ;; complex number
        (t (op2ml (caar exp) (cdr exp)))
  )
)

(defun op2ml(op args)
(let ((sym (get op 'ctfun)))
  (cond (sym (row-begin "<apply>")
	     (tprinc "<")(tprinc sym)(tprinc ">") 
	)
        (t (row-begin "<apply>")
	   (tprinc "<fn>")(tprinc (stripdollar op))
	   (tprinc "</fn>") 
        )
  )
  (mapc (function ctmathml) args)
  (row-end "</apply>")
))

(defun ctarray(a)  ;; subscripted var
  (tprinc "<ci>") (row-begin "")(mPr-array a)
  (row-end "")
  (tprinc "</ci>")
)
  
(defun a2ml(a)    ;; treat atoms
(prog(val)
  (cond ((numberp a) 
 	 (tprinc "<cn")
         (cond ((or (fixnump a) (bignump a))
                (tprinc " type=\"integer\">"))
               ((or (floatp a) (bigfloatp a))
	        (tprinc " type=\"float\">"))
		(t (tprinc ">"))
	 )
         (tprinc (princ-to-string a))
	 (tprinc "</cn>")
        )
        ((setq val (safe-get a 'chchr))
	 (cond ((member val '("&pi;" "&gamma;" "&ii;" "&ee;") :test #'equal)
	          (tprinc "<cn type=\"constant\">") )
	       (t (tprinc "<cn>") )
	 )
         (tprinc val) (tprinc "</cn>")
        )
        (t
          (let ((my-atom (if (symbolp a) (print-invert-case (stripdollar a)) a)))
            (tprinc "<ci>")
            (tprinc (coerce (mapcar #'handle_rsw (rm '// (exploden my-atom))) 'string))
            (tprinc "</ci>"))))))

(defun cpxp(a)
(if (among '$%i a)
 (let ( (r($realpart a)) (i ($imagpart a)) )
  (cond ((and (numberp r) (numberp i))
 	 (tprinc "<cn type=\"complex\">")
	 (tprinc r) (tprinc "<sep/>")
	 (tprinc i) (tprinc "</cn>")
	 t)
	(t nil)
  )
 )
))

(defun fractionp(a)
  (cond ((and (eq (caar a) 'rat)
	      (integerp (cadr a))
	      (integerp (caddr a))
	 )
 	 (tprinc "<cn type=\"rational\">")
	 (tprinc (cadr a)) (tprinc "<sep/>")
	 (tprinc (caddr a)) (tprinc "</cn>")
	 t)
	(t nil)
  )
)

(defun ctlist(op args)
   (tprinc "<list>")
   (mapc (function ctmathml) args)
   (tprinc "</list>")
)

(defun matrixrow(args)
   (setq args (cdr args))
   (row-begin "<matrixrow>")
   (mapc (function ctmathml) args)
   (row-end "</matrixrow>")
)

(defun ctmatrix(op args)
   (row-begin "<matrix>")
   (mapc (function matrixrow) args)
   (row-end "</matrix>")
)

(defun ctvector(op args)
   (tprinc "<vector>")
   (mapc (function ctmathml) args)
   (tprinc "</vector>")
)

(defun relation(op args)
(let ((sym (get op 'ctfun)))
   (row-begin "<reln>") (tprinc "<")(tprinc sym)(tprinc ">") 
   (mapc (function ctmathml) args)
   (row-end "</reln>")
))

(defun sumprod(op args)
   (cond ((equal (length args) 4)
	  (let ((sym (get op 'ctfun)) 
		(exp (car args)) (var (cadr args))
		(ll (caddr args)) (ul (cadddr args)))
	       (row-begin "<apply>")
	       (tprinc "<")(tprinc sym)(tprinc ">") 
	       (tprinc "<bvar>")(ctmathml var)(tprinc "</bvar>") 
               (setq ll (nformat (meval 
                 (list '($substitute) '((mminus) $inf) '$minf ll))))
	       (tprinc "<lowlimit>")(ctmathml ll)(tprinc "</lowlimit>") 
	       (myterpri)
               (setq ul (nformat (meval 
                 (list '($substitute) '((mminus) $inf) '$minf ul))))
	       (tprinc "<uplimit>")(ctmathml ul)(tprinc "</uplimit>") 
	       (ctmathml exp)
	       (row-end "</apply>")
	  ))
	  (t (tprinc "sumprod: Wrong args")))
)

(defun ctlimit(op args)
(let ((sym (get op 'ctfun)) (f (car args))
      (v (cadr args)) (p (caddr args))) 
     (setq args (cdddr args))
     (row-begin "<apply>")
     (tprinc "<")(tprinc sym)(tprinc ">") 
     (tprinc "<bvar>")(ctmathml v)(tprinc "</bvar>") 
     (setq p (nformat (meval 
       (list '($substitute) '((mminus) $inf) '$minf p))))
     (tprinc "<lowlimit>")(ctmathml p)(tprinc "</lowlimit>") 
     (myterpri)
     (cond (args (row-begin "<condition>")
	         (cond ((eq (car args) '$plus)
			(relation 'mgreaterp (list v 0)))
		       (t (relation 'mgreaterp (list v 0)))
                 )
		 (row-end "</condition>") 
     ))
     (ctmathml f)(row-end "</apply>")
))

(defun ctdiff(op args)
(let ((sym (get op 'ctfun)) (f (car args))) 
   (setq args (cdr args))
   (row-begin "<apply>")
   (tprinc "<")(tprinc sym)(tprinc ">") 
   (ctmathml f)
   (cond ((equal (length args)  1)
          (tprinc "<bvar>")(ctmathml (car args))
	  (tprinc "</bvar>") 
	 )
         (t (do ((vl args (cddr vl)))
	        ((null vl) nil)
	        (diffvar (car vl) (cadr vl))
	 ))
   )
   (row-end "</apply>") 
))

(defun diffvar(v d)
  (tprinc "<bvar>")(ctmathml v)
  (tprinc "<degree>") (ctmathml d)
  (tprinc "</degree>") 
  (tprinc "</bvar>") 
  (myterpri)
)
 
(defun ctintegrate(op args)
   (cond ((equal (length args)  4)
	  (sumprod op args))
	 (t (let ((sym (get op 'ctfun)) 
		  (exp (car args)) (var (cadr args)))
	       (row-begin "<apply>")
	       (tprinc "<")(tprinc sym)(tprinc ">") 
	       (tprinc "<bvar>")(ctmathml var)(tprinc "</bvar>") 
	       (ctmathml exp)
	       (row-end "</apply>")
	    ))
))


(defun lamd(vars def)
  (row-begin "<lambda>")
  (do ((l vars (cdr l)))
      ((null l) nil)
      (tprinc "<bvar>")(ctmathml (car l)) (tprinc "</bvar>")
  )
  (ctmathml def)
  (row-end "</lambda>")
)

(defun def-fun (op args)
(let ((fn (car args)) (def (cadr args)))
    (row-begin "<declare type=\"fn\">")
    (ctmathml (caar fn))
    (lamd (cdr fn) def)
    (row-end "</declare>")
))

;;;;;;;;;;; tables ;;;;;;;;;;;;
(setup '(%sin  (ctfun "sin/")))
(setup '(%cos  (ctfun "cos/")))
(setup '(%tan  (ctfun "tan/")))
(setup '(%cot  (ctfun "cot/")))
(setup '(%sec  (ctfun "sec/")))
(setup '(%csc  (ctfun "csc/")))

(setup '(%asin  (ctfun "arcsin/")))
(setup '(%acos  (ctfun "arccos/")))
(setup '(%atan  (ctfun "arctan/")))
(setup '(%acot  (ctfun "acot/")))
(setup '(%asec  (ctfun "asec/")))
(setup '(%acsc  (ctfun "acsc/")))
(setup '(%sinh  (ctfun "sinh/")))
(setup '(%cosh  (ctfun "cosh/")))
(setup '(%tanh  (ctfun "tanh/")))
(setup '(%coth  (ctfun "coth/")))
(setup '(%sech  (ctfun "sec/")))
(setup '(%csch  (ctfun "csch/")))


(setup '(%asinh  (ctfun "asinh/")))
(setup '(%acosh  (ctfun "acosh/")))
(setup '(%atanh  (ctfun "atanh/")))
(setup '(%acoth  (ctfun "acoth/")))
(setup '(%asech  (ctfun "asec/")))
(setup '(%acsch  (ctfun "acsch/")))

(setup '(%ln  (ctfun "ln/")))
(setup '(%log  (ctfun "log/")))

(setup '($sin  (ctfun "sin/")))
(setup '($cos  (ctfun "cos/")))
(setup '($tan  (ctfun "tan/")))
(setup '($cot  (ctfun "cot/")))
(setup '($sec  (ctfun "sec/")))
(setup '($csc  (ctfun "csc/")))

(setup '($asin  (ctfun "arcsin/")))
(setup '($acos  (ctfun "arccos/")))
(setup '($atan  (ctfun "arctan/")))
(setup '($acot  (ctfun "acot/")))
(setup '($asec  (ctfun "asec/")))
(setup '($acsc  (ctfun "acsc/")))

(setup '($sinh  (ctfun "sinh/")))
(setup '($cosh  (ctfun "cosh/")))
(setup '($tanh  (ctfun "tanh/")))
(setup '($coth  (ctfun "coth/")))
(setup '($sech  (ctfun "sec/")))
(setup '($csch  (ctfun "csch/")))

(setup '($asinh  (ctfun "asinh/")))
(setup '($acosh  (ctfun "acosh/")))
(setup '($atanh  (ctfun "atanh/")))
(setup '($acoth  (ctfun "acoth/")))
(setup '($asech  (ctfun "asec/")))
(setup '($acsch  (ctfun "acsch/")))
(setup '($ln  (ctfun "ln/")))
(setup '($log  (ctfun "log/")))


;;;;; containers
(setup '(mlist (ct-proc ctlist)))
(setup '($matrix (ct-proc  ctmatrix)))
(setup '($vector (ct-proc  ctvector)))

;;;;;;; Operators and functions
(setup '(mand  (ctfun "and/")))
(setup '(mor  (ctfun "or/")))
(setup '(mnot  (ctfun "not/")))
(setup '($xor  (ctfun "xor/")))

(setup '(mplus  (ctfun "plus/")))
(setup '($plus (ctfun "plus/")))
(setup '(mminus  (ctfun "minus/")))
(setup '($minus (ctfun "minus/")))
(setup '(mdif  (ctfun "minus/")))
(setup '($remainder  (ctfun "rem/")))
(setup '($max  (ctfun "max/")))
(setup '($min  (ctfun "min/")))
(setup '(mfactorial  (ctfun "factorial/")))
(setup '(mabs (ctfun "abs/")))
(setup '(%abs (ct-proc abs)))
(setup '(mnctimes  (ctfun "times/ type=\"noncommutative\"")))
(setup '(mtimes  (ctfun "times/")))
(setup '(mexpt (ctfun "power/")))
;;(setup '(mdottimes (ctfun "&CenterDot;")))
(setup '(mquotient (ctfun "quotient/"))) 
(setup '(rat (ct-proc rat)))
(setup '($sqrt (ctfun "sqrt/")))
(setup '(%sqrt (ctfun "sqrt/")))

(setup '(mquote  (ctfun "quote/")))


(setup '(mgreaterp  (ct-proc relation) (ctfun "gt/")))
(setup '(mgeqp (ct-proc relation)  (ctfun "geq/")))
(setup '(mequal (ct-proc relation)  (ctfun "eq/")))
(setup '(mnotequal (ct-proc relation)  (ctfun "neq/")))
(setup '(mleqp (ct-proc relation)  (ctfun "leq/")))
(setup '(mlessp (ct-proc relation)  (ctfun "lt/")))

(setup '(mdefine (ct-proc def-fun)))

;;(setup '(msetq  (ctfun "&Assign;")))
;;(setup '(mset  (ctfun "&Assign;")))  ;;; This is not math
;;(setup '(marrow  (ctfun "&RightArrow;")))
;;(setup '(mrarrow  (ctfun "&RightArrow;")))
;;(setup '(%at (ct-proc mPr-at)))
;;(setup '($at (ct-proc mPr-at)))
;;(setup '($det (ct-proc mPr-det)))
;;(setup '(%determinant (ct-proc det)))
;;(setup '($binomial (ct-proc binomial)))
;;(setup '(%binomial (ct-proc binomial)))

(setup '(%sum (ct-proc sumprod)(ctfun "sum/")))
(setup '($sum (ct-proc sumprod)(ctfun "sum/")))
(setup '($product (ct-proc sumprod)(ctfun "product/")))
(setup '(%product (ct-proc sumprod)(ctfun "product/")))
(setup '($integrate (ct-proc ctintegrate)(ctfun "int/")))
(setup '(%integrate (ct-proc ctintegrate)(ctfun "int/")))
(setup '($diff (ct-proc ctdiff)(ctfun "diff/")))
(setup '(%derivative (ct-proc ctdiff)(ctfun "diff/")))
(setup '($limit (ct-proc ctlimit)(ctfun "limit/")))
(setup '(%limit (ct-proc ctlimit)(ctfun "limit/")))

;;(setup '(mprog (ctfun "block")))
;;(setup '($block (ctfun "block")))
;;(setup '($$boldif (ctfun "if/")))
;;(setup '($$boldthen (ctfun "then/")))
;;(setup '($$boldelse (ctfun "else/")))
