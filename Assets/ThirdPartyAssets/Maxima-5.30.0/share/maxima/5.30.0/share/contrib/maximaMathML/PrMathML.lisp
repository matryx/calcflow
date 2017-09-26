
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Purpose:  Generate Presentation MathML code from MAXIMA
;;;  File: PrMathML.lsp
;;;  Author: Paul S. Wang
;;;  Date: March 1999
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;=============================================================================
;    (c) copyright 1999  Kent State University
;               all rights reserved.
;=============================================================================
(in-package :maxima)
(macsyma-module mathml)

;; special variables used in TeXetting
(declaim (special *row* *indent* ccol mPrport $mPrautolabel $mPrworksheet $lamPrworksheet
             $mPrlabelleft $lamPrautolabel $mPrdisplaytype $mPrevaluate
             macmPr-lib lop rop $labels casep))

;;****************************************************************************
;;      Program : prmathml
;;****************************************************************************
;;Generatig MathML presenation codes for the expr 
;;This is a maxima top-level function used  the form
;;              prmathml(expr, [,filename[,t (d)]])  on the C-line

(defmfun $prmathml (&rest margs)
         (prog (ccol *row* *indent* filename mexplabel mexpress mPrport x y)
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
           (cond
             ((member mexpress $labels :test #'eq)
              (setq mexplabel
                    (intern (concatenate 'string "("
                                (princ-to-string (fullstrip1 mexpress))
                                ")")))
              (setq mexpress (eval mexpress)))
             (t (setq mexplabel nil)
                (when $mPrevaluate (setq mexpress (meval mexpress)))))
           (when $mPrautolabel (setq mexplabel (updateautolabel)))
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
                            (caddr y))))))
           (when (and (consp mexpress) (consp (car mexpress))
                      (eq 'mlabel (caar mexpress)))
             (setq mexpress (cadr mexpress)))
           (cond
             ((and $lamPrworksheet
                   (when mexplabel
                     (member 'c (explode mexplabel) :test #'eq)))
              (format mPrport "\\begin{verbatim}~%~a " mexplabel)
              (mgrind mexpress mPrport)
              (format mPrport ";~%\\end{verbatim}~%"))
             ((and $mPrworksheet
                   (when mexplabel
                     (member 'c (explode mexplabel) :test
                             #'eq)))
              (format mPrport "|~a " mexplabel)
              (mgrind mexpress mPrport) (format mPrport ";|~%"))
             (t (cond
                  ($lamPrautolabel
                      (format mPrport "\\begin{equation}~%"))
                  ($mPrdisplaytype 
		    (tprinc "<math  xmlns='http://www.w3.org/1998/Math/MathML'>") )
                  (t (tprinc "<math  xmlns='http://www.w3.org/1998/Math/MathML'>")))
                (mPr_engine mexpress 'mparen 'mparen)
                (cond
                  ($lamPrautolabel
                      (format mPrport "~%\\end{equation}~%"))
                  ($mPrdisplaytype
                      (when mexplabel
                        (if $mPrlabelleft
                            (format mPrport "\\leqno{\\tt ~a}" mexplabel)
                            (format mPrport "\\eqno{\\tt ~a}" mexplabel)))
                        (tprinc "</math>") (myterpri))
                  (t (tprinc "</math>")))))
           (when filename (terpri mPrport) (close mPrport))
           (return 'done)))



;;      mPr_engine is a kernel fuction for this program. It checks whether
;;an argument "mexpress" is an atom or expression. Then it will assign
;;a proper function to the expression or just print if it is an atom.
;;This is an applied object-oriented programming technique.

;;      arg: mexpress - macsyma internal representaton
;;           lop , rop - left and right handside operator of mexpress

;;special check if expression is an array
;;check whether or not to put parenthesis 
;;if not a keyword,it is a function

;;;;;; This is the work house routine ;;;;;;;;

(defun mPr_engine (mexpress lop rop)
  (setq mexpress (nformat mexpress))
  (if (atom mexpress) (mPr-atom mexpress)
      (when (listp (car mexpress))
        (cond
          ((member 'array (car mexpress) :test #'eq)
           (mPr-array mexpress))
          ((or (<= (mPr-lbp (caar mexpress)) (mPr-rbp lop))
               (> (mPr-lbp rop) (mPr-rbp (caar mexpress))))
           (mPr-paren mexpress))
          (t (if (get (caar mexpress) 'mPrprocess)
                 (funcall (get (caar mexpress) 'mPrprocess) mexpress)
                 (mPr-function mexpress)))))))



;*************************************************************************
;;
;;                              Utilities Section
;;
;*************************************************************************

;;; tprinc is an intelligent low level printing routine.  it keeps track of 
;;; the size of the output for purposes of allowing the TeX file to
;;; have a reasonable line-length. tprinc will break it at a space 
;;; once it crosses a threshold.
;;; this has nothign to do with breaking the resulting equations.
 
;-      arg:    chstr -  string or number to princ
;-      scheme: This function keeps track of the current location
;-              on the line of the cursor and makes sure
;-              that a value is all printed on one line (and not divided
;-              by the crazy top level os routines)
 
(defun row-begin(str)
  (myterpri)
  (princ str  mPrport)
  (incf *indent*)
  (setq *row* t))

(defun myindent()
  (do ((i *indent*))
      ((equal i 0) nil)
    (princ "   " mPrport)
    (decf i)))

(defun row-end(str)
  (decf *indent*)
  (myterpri)
  (princ str mPrport)
  (setq *row* t))

(defun tpchar (c)
  (incf ccol)
  (princ c mPrport))

;would have exceeded the line length
; lead off with a space for safety
;so we split it up.
(defun tprinc (chstr)
  (prog (chlst linebreak)
    (cond ((> (+ (length (setq chlst (exploden chstr))) ccol) 80)
        (setq linebreak t)))
    (cond (*row* (setq *row* nil) (myterpri))) ;; *row* calls for new row
    (do ((ch chlst (cdr ch)) (colc ccol (1+ colc)))
        ((null ch) (setq ccol colc))
      (write-char (car ch) mPrport)
      (if (and linebreak (eq (car ch) '>))  ;; line break only after >
	  (myterpri))
)))

;;   myterpri is terpri with port and indent control

(defun myterpri ()
  (if mPrport (terpri mPrport) (mterpri))
  (setq ccol 1)
  (myindent)
)

;;      lastlementp is a predicate function to check a list l
;;that is there only one element left in the list.

(defun lastelementp (l) (equal (length l) 1))

;;      getsymbol is a function tool. It'll  get the information
;;from the database which is a symbol for an argument (atom)

(defun getsymbol (atom) (get atom 'chchr))

;;      get_process is a function tool. It'll  get the information
;;from the database about the process to handle the operator (atom)
;; (defun get_process (atom) (get atom 'mPrprocess))

;;      setup is a function to build the database (put properties) for 
;;each key word

; check if property exists already
(defun setup (arg)
  (mapc #'(lambda (ls) (setf (get (car arg) (car ls)) (cadr ls)))
        (cdr arg)))

;;      mPr-lbp and mPr-rbp are the functions to get information
;;      about size of the particular operator
;;      this is from the latex version of this prog
;;      not sure how well it works for MathML

(defun mPr-lbp (x) (cond ((get x 'mPr-lbp)) (t (lbp x))))

(defun mPr-rbp (x) (cond ((get x 'mPr-rbp)) (t (rbp x))))

;; reduce lbp and rbp value for mtimes to get less parentesis
(defun $lessparen ()
  (setf (get 'mtimes 'mPr-lbp) '110)
  (setf (get 'mtimes 'mPr-rbp) '110)
  '$done)
;; get back to normal case for paren
(defun $parenback ()
  (setf (get 'mtimes 'mPr-lbp) '120)
  (setf (get 'mtimes 'mPr-rbp) '120)
  '$done)

;;      mPr-abs is a function to handle abs()

(defun mPr-abs (mexpress)
  (tprinc "<mo>|</mo>")
  (mPr_engine (cadr mexpress) 'mparen 'mparen)
  (tprinc "<mo>|</mo>"))

;; a[1]^2 or a[x,y]^z
(defun mPr-arr-power(b e)
  (tprinc "<msubsup>")
  (mPr_engine (caar b) lop 'mfunction)
  (cond
   ((equal (length b) 2) 
     (mPr_engine (cadr b) lop rop)
   )
   (t (row-begin "<mrow>") ;;(tprinc "<mrow>")
     (do ((l (cdr b) (cdr l))) ((null l)(row-end "</mrow>"))
       (mPr_engine (car l) lop rop)
       (when (not (lastelementp l)) (tprinc "<mo>,</mo>")))
  ))
  (mPr_engine e 'mparen 'mparen) (tprinc "</msubsup>")
)

;;      when the operator is array ,this function will be called
;;      ex. a[x1,..] is a top level representation

(defun mPr-array (mexpress)
  (tprinc "<msub>")
  (mPr_engine (caar mexpress) lop 'mfunction)
  (row-begin "<mrow>") 
  (do ((l (cdr mexpress) (cdr l))) ((null l) (row-end "</mrow>")(tprinc "</msub>"))
    (mPr_engine (car l) lop rop)
    (when (not (lastelementp l)) (tprinc "<mo>,</mo>"))))

;;      mPr-at is a function to handel at(..) function 
;;
(defun mPr-at (mexpress)
  (row-begin "<mrow>") ;;(tprinc "<mrow>")
  (mPr_engine (cadr mexpress) lop rop)
  (tprinc "<msub><mo>|</mo>")
  (mPr_engine (caddr mexpress) 'mparen 'mparen)
  (tprinc "</msub>") (row-end "</mrow>")
)
;;      in mPr_engine ,whennever mexpress is an atom this function taking care 
;;of it by getting a TeX symbol if it exsits. Also it handles some word wich
;;has a reserved character for TeX

;; prints instead of returning value now
(defun mPr-atom (chr)
  (cond
    ((numberp chr) (mPr-num chr))
    ;; pwang 1/2005
    ;; ((atom chr) (tprinc "<mi>") (tprinc (fullstrip1 chr)) (tprinc "</mi>"))
    ((safe-get chr 'chchr) (tprinc "<mi>") 
     (tprinc (safe-get chr 'chchr)) (tprinc "</mi>"))
    (t
      (let ((my-atom (if (symbolp chr) (print-invert-case (stripdollar chr)) chr)))
        (tprinc "<mi>")
        (tprinc (coerce (mapcar #'handle_rsw (exploden my-atom)) 'string))
        (tprinc "</mi>")))))

(defun rm (a list)
  (do ((l list (cdr l)) (l2 nil)) ((null l) (reverse l2))
    (when (not (equal a (car l))) (setq l2 (cons (car l) l2)))))


;;      this fn is called by mPr-atom ,it checks for a reserved char.
(defun handle_rsw (c)
  (cond
    ((equal c #\<) "&lt;")
    ((equal c #\>) "&gt;")
    ((equal c #\&) "&amp;")
    (t c)))

;;      mPr-binomial :-
;;       top level:  binomail(x,y);

(defun mPr-binomial (mexpress)
  (row-begin "<mrow>") (tprinc "<mo>(</mo><mfrac linethickness=\"0\">")
  (mPr_engine (cadr mexpress) 'mparen 'mparen)
  (tprinc "  ")
  (mPr_engine (caddr mexpress) 'mparen 'mparen)
  (tprinc "</mfrac><mo>)</mo>") (row-end "</mrow>"))



;;      mPr-det is a function to handle determinant()

(defun mPr-det (mexpress)
  (let ((operand (cadr mexpress)))
    (tprinc "<mi>det</mi>")
    (mPr_engine operand 'mparen 'mparen)))

;;      mPr-dif is a function to handle diferentiation function. 
;;It calls to subfunctions powerof_d and denopart.
;;
(defun mPr-diff (mexpress)
  (cond
    ((powerof_d (cddr mexpress)) (denopart (cddr mexpress))
     (tprinc "</mfrac><mo>&InvisibleTimes;</mo>") 
     (mPr_engine (cadr mexpress) 'mtimes rop)
     (row-end "</mrow>"))
    (t (mPr_engine (cadr mexpress) lop rop))))

;;if there is no repeating differentiation
;; just diff(exp,x)
;; if diff(exp,x,no,..)
(defun powerof_d (l)
  (cond
    ((lastelementp l) 
     (row-begin "<mrow>")(tprinc "<mfrac><mo> &dd; </mo>") t)
    (t (do ((l1 l (cddr l1)) (l2 nil (cons (cadr l1) l2))
            (power_of_d nil))
           ((null l1) (setq power_of_d (addn l2 nil))
            (cond
              ((numberp power_of_d)
               (cond
                 ((equal 0 power_of_d) nil)
                 ((equal 1 power_of_d)(row-begin "<mrow>")(tprinc "<mfrac><mo> &dd; </mo>") t)
                 (t (row-begin "<mrow>")(tprinc "<mfrac><msup><mo> &dd;</mo><mn>")
		    (tprinc power_of_d) (tprinc "</mn></msup>") t)))
              (t (row-begin "<mrow>")(tprinc "<mfrac><msup><mo> &dd;</mo>")
                 (mPr_engine power_of_d 'mparen 'mparen)
                 (tprinc "</msup>") t))))))
)

;;if just diff(exp,x)
;;if diff(exp,x,nox,y,noy,...)
(defun denopart (l)
  (prog (result)
    (cond
      ((lastelementp l) (row-begin "<mrow>") (tprinc "<mo> &dd; </mo>")
       (p-op (getsymbol 'mtimes)) (mPr_engine (car l) 'mtimes rop)
      (row-end "</mrow>"))
      (t (do ((l1 l (cddr l1)) (l2 nil)) ((null l1) (setq result l2))
           (setq l2
                 (cons (append '((mexpt)) (list (car l1))
                               (list (cadr l1)))
                       l2)))
         (setq result (muln result nil))
         (cond
           ((atom result) (row-begin "<mrow>")(tprinc "<mo> &dd; </mo>")
	    (p-op (getsymbol 'mtimes))
            (mPr_engine result 'mparen 'mparen) (row-end "</mrow>"))
           ((listp result)
            (cond
              ((eq (caar result) 'mexpt) 
	       (row-begin "<mrow>")(tprinc "<mo> &dd; </mo>")
               (p-op (getsymbol 'mtimes))
               (mPr_engine result 'mtimes 'mparen) (row-end "</mrow>"))
              (t (row-begin "<mrow>") 
                 (do ((l1 (cdr result) (cdr l1)) (l2 nil)
                      (power_of_d nil))
                     ((null l1) (row-end "</mrow>"))
                   (tprinc "<mo> &dd; </mo>")
                   (p-op (getsymbol 'mtimes))
                   (mPr_engine (car l1) 'mtimes 'mtimes)
                   (when (not (lastelementp l1)) (tprinc "<mo>,</mo>"))
		 )
		 ))))))))
        
;; this fuction is adopted the main idea form macTeX from Prof. Richard
;; Fateman in the mPr-mexpt
;;
;; insert left-angle-brackets for mncexpt. a^<n> is how a^^n looks.
;; here is where we have to check for f(x)^b to be displayed
;; as f^b(x), as is the case for sin(x)^2 .
;; which should be sin^2 x rather than (sin x)^2 or (sin(x))^2. 
;; yet we must not display (a+b)^2 as +^2(a,b)...
;; or (sin(x))^(-1) as sin^(-1)x, which would be arcsine x
; this is f(x)
; this is f [or nil]
;this is (x) [maybe (x,y..), or nil]
;; this is the exponent
; there is such a function
;; insist it is a % or $ function
;     x
;;this case like sin(x)^x --> sin x
;; if for example exp = (x+2)^4
;;      in case x^^y
(defun mPr-expt (mexpress)
 (cond
  ((eq (caar mexpress) 'mexpt)
   (if (and (not (atom (cadr mexpress)))
	     (member 'array (caadr mexpress) :test #'eq) ;; array
       )
       (mPr-arr-power (cadr mexpress) (caddr mexpress))
       (let* ((fx (cadr mexpress))
            (f (and (not (atom fx)) (atom (caar fx)) (caar fx)))
            (bascdr (and f (cdr fx))) (expon (caddr mexpress))
            (doit (and f (member (char (string f) 0) (list #\% #\$) :test #'eq)
                       (not (member f '(%sum %product) :test #'eq)))))
        (cond
           (doit (cond     ;;;; sin^2 x case
                 ((atom expon) 
		  (row-begin "<mrow>")(tprinc "<msup>") (mPr-fname f)
                  (mPr-atom expon)(tprinc "</msup>") 
                  (tprinc "<mo>&ApplyFunction;</mo>")
                  (if (cdr bascdr) (mPr-listparen bascdr)
                      (mPr_engine (car bascdr) 'mtimes 'mtimes))
		  (row-end "</mrow>")
		 )
                 (t (tprinc "<msup>")
		    (mPr_engine fx 'mparen 'mparen)
                    (mPr_engine expon 'mparen 'mparen)
                    (tprinc "</msup>"))))
          (t (tprinc "<msup>")
            (mPr_engine (cadr mexpress) lop (caar mexpress))
            (mPr_engine (caddr mexpress) 'mparen 'mparen)
            (tprinc "</msup>")))))
  )
  (t (tprinc "<msup>") ;;;  mnexpt case
       (mPr_engine (cadr mexpress) lop (caar mexpress))
       (row-begin "<mrow>")(tprinc "<mo>&LeftAngleBracket;</mo>")
       (mPr_engine (caddr mexpress) 'mparen 'mparen)
       (tprinc "<mo>&RightAngleBracket;</mo>")(row-end "</mrow>")
       (tprinc "</msup>")))
)
;;      this function will check that whether or not an arg has a symbol
;;in data base or not, if not it 'll be treated to be function which 'll
;;be printed in rm font
(defun mPr-fname (f)
     (tprinc "<mi>")
     (tprinc (if (getsymbol f) (getsymbol f) f))
     (tprinc "</mi>")
)
;;      to handle if an operator is a function which will be printed
(defun mPr-function (mexpress)
  (mPr_engine (caar mexpress) 'mparen 'mparen)
  (tprinc "<mo>&ApplyFunction;</mo>")
  (mPr-listparen (cdr mexpress)))

;;      for infix operator , and also handle when there is a truncation
;;in macsyma expression (see mPr-infix1)
;;      mPr-infix calling
;;              1)mPr-infix1 calling 
;;                      1.1) p-op-oprd
;;              2)p-op-oprd
;;
;if -x or +x so call mPr-function
(defun mPr-infix (mexpress)
  (let ((moperator (car mexpress)) (moperands (cdr mexpress)))
    (cond
      ((equal (length moperands) 1) (mPr-function mexpress))
      (t (row-begin "<mrow>")
	 (mPr_engine (car moperands) lop (car moperator))
         (p-op-oprd moperator (cadr moperands))
         (mPr-infix1 moperator (cddr moperands))
	 (row-end "</mrow>")
))))

(defun mPr-infix1 (moperator moperands)
  (cond
    ((null moperands)
     (when (member 'trunc moperator :test #'equal)
       (p-op (getsymbol (car moperator)))
       (tprinc "<mo>&TripleDot;</mo>")))
    (t (p-op-oprd moperator (car moperands))
       (mPr-infix1 moperator (cdr moperands)))))

;;      p-op-oprd is a function printing operator and operand consecutively
;;      ex      + x     when + is a infix op and x is oprd

(defun p-op-oprd (moperator moperand)
  (let ((op (car moperator)))
    (cond
      ((equal op 'mplus)
       (cond
         ((listp moperand)
          (cond
            ((equal (caar moperand) 'mminus) (tprinc "<mo>-</mo>")
             (mPr_engine (cadr moperand) 'mminus rop))
            (t (tprinc "<mo>+</mo>") (mPr_engine moperand 'mplus rop))))
         (t (tprinc "<mo>+</mo>") (mPr-atom moperand))))
      (t (p-op (getsymbol op)) 
         (mPr_engine moperand op op))))
)

(defun p-op(symbol)
    (tprinc "<mo>") 
    (tprinc symbol) 
    (tprinc "</mo>") 
)

;;      mPr-intgrate handles an integration expression
;;      It will detect that integrate function is called in short form
;; or long form example: integrate(x^4,x,0,inf) is a long form.

;;short form
;;long form
(defun mPr-integrate (mexpress)
  (setq mexpress (meval 
    (list '($substitute) '((mminus) $inf) '$minf mexpress)))
  (cond
    ((equal (length mexpress) 3)
     (row-begin "<mrow>")(tprinc "<mo>&Integral;</mo>"))
    ((equal (length mexpress) 5)
     (row-begin "<mrow>")(tprinc "<msubsup><mo>&Integral;</mo>")
     (mPr_engine (cadddr mexpress) 'mparen 'mparen) (tprinc "  ")
     (mPr_engine (car (cddddr mexpress)) 'mparen 'mparen)
     (tprinc "</msubsup>"))
    (t (merror "Wrong NO. of Arguments")))
  (row-begin "<mrow>")
  (mPr_engine (cadr mexpress) 'mparen 'mparen)
  (row-end "</mrow>")(tprinc "<mo>&InvisibleTimes;</mo><mo> &dd; </mo>")
  (mPr_engine (caddr mexpress) 'mparen rop)
  (row-end "</mrow>"))

                      
;;      mPr-limit takes care the "limit(exp,var,val,dir)"
(defun mPr-limit (mexpress)
  (setq mexpress (meval 
    (list '($substitute) '((mminus) $inf) '$minf mexpress)))
  (row-begin "<mrow>")(tprinc "<munder><mo>lim</mo>")(row-begin "<mrow>")
  (mPr_engine (caddr mexpress) 'mparen 'mparen)
  (tprinc "<mo>&RightArrow;</mo>")
  (mPr_engine (cadddr mexpress) 'mparen 'mapren)
  (when (car (cddddr mexpress))
    (if (member (car (cddddr mexpress)) '($minus $plus) :test #'equal)
        (p-op (getsymbol (car (cddddr mexpress))))
        (merror "THE 4TH ARG MUST BE PLUS OR MINUS")))
  (row-end "</mrow>")(tprinc "</munder>")
  ;; (tprinc "<mo>&InvisibleTimes;</mo>")
  (mPr_engine (cadr mexpress) 'mparen rop)
  (row-end "</mrow>"))
;;      This function handles a macsyma list expression 
;; 
(defun mPr-list (mexpress)
  (tprinc "<mo>[</mo>")
  (do ((l (cdr mexpress) (cdr l))) ((null l) (tprinc "<mo>]</mo>"))
    (mPr_engine (car l) 'mparen 'mparen)
    (when (not (lastelementp l)) (tprinc "<mo>,</mo>"))))


;;      This function is a subfunction of mPr-expt , mPr-function and
;; mPr-mqapply
(defun mPr-listparen (mexpress)
  (row-begin "<mrow>")(tprinc "<mo>(</mo>")
  (do ((l mexpress (cdr l))) ((null l) (tprinc "<mo>)</mo>") (row-end "</mrow>"))
    (mPr_engine (car l) 'mparen 'mparen)
    (when (not (lastelementp l)) (tprinc "<mo>,</mo>"))))

;;      mPr-matrix handles matrix function
(defun mPr-matrix (mexpress)
  (row-begin "<mfenced open='(' close=')'><mtable>")
  (mapc #'(lambda (arg)
	    (row-begin "<mtr>")
            (do ((l (cdr arg) (cdr l))) ((null l) (row-end "</mtr>"))
	      (row-begin "<mtd>")
	      (mPr_engine (car l) 'mparen 'mparen)
	      (row-end "</mtd>")
	      ))
        (cdr mexpress))
  (row-end "</mtable></mfenced>")
)

(defun mPr-mqapply (mexpress)
  (mPr_engine (cadr mexpress) lop 'mfunction)
  (mPr-listparen (cddr mexpress)))

;; this function handles the floating point number.
;; convert 1.2e20 to 1.2 \\cdot 10^{20}
;; is it ddd.ddde+EE
;; it is not. go with it as given
(defun mPr-num (atom)
  (let (r firstpart exponent)
    (cond
      ((integerp atom) (tprinc "<mn>") (tprinc atom) (tprinc "</mn>"))
      (t (setq r (explode atom))
         (setq exponent (member 'e r :test #'eq))
         (cond
           ((null exponent) (tprinc "<mn>") (tprinc atom) (tprinc "</mn>"))
           (t (setq firstpart
                    (nreverse (cdr (member 'e (reverse r) :test #'eq))))
              (tprinc "<mn>") 
	      (mapc #'tpchar firstpart) (tprinc "</mn>") 
	      (tprinc "<mo>&CenterDot;</mo><msup><mn>10</mn> <mn>")
              (mapc #'tpchar (cdr exponent))
	      (tprinc "</mn></msup>")))))))

;;      this function puts parenthesis for the expression
(defun mPr-paren (mexpress)
  (row-begin "<mrow>")(tprinc "<mo>(</mo>")
  (mPr_engine mexpress 'mparen 'mparen)
  (tprinc "<mo>)</mo>") (row-end "</mrow>"))
;;      this function handles "+" operator which is infix form
;;
;if -x or +x so call mPr-function
(defun mPr-plus (mexpress)
  (let ((moperands (cdr mexpress))
        (flag_trunc (member 'trunc (car mexpress) :test #'eq)))
    (cond
      ((equal (length moperands) 1) (mPr-prefix mexpress))
      (t (row-begin "<mrow>")
	 (mPr_engine (car moperands) lop 'mplus)
         (print_op_oprd (cadr moperands))
         (mPr-plus1 (cddr moperands) flag_trunc)
         (row-end "</mrow>")
      )
)))

(defun mPr-plus1 (moperands flag_trunc)
  (cond
    ((null moperands) (when flag_trunc (tprinc "<mo>+</mo><mo>&TripleDot;</mo>")))
    (t (print_op_oprd (car moperands))
       (mPr-plus1 (cdr moperands) flag_trunc))))


(defun print_op_oprd (moperand)
  (cond
    ((listp moperand)
     (cond
       ((equal (caar moperand) 'mminus) (tprinc "<mo>-</mo>")
        (mPr_engine (cadr moperand) 'mplus rop))
       (t (tprinc "<mo>+</mo>") (mPr_engine moperand 'mplus 'mparen))))
    (t (tprinc "<mo>+</mo>") (mPr-atom moperand))))
;;      mPr-postfix handles for postfix notation expression like factorial
;;
(defun mPr-postfix (mexpress)
  (row-begin "<mrow>")
  (mPr_engine (cadr mexpress) lop (caar mexpress))
  (row-end "</mrow>") 
  (p-op (getsymbol (caar mexpress))))

;;      mPr-prefix is a function to handle a prefix notation form
;;
(defun mPr-prefix (mexpress)
  (let ((op (caar mexpress)) (oprnd (cadr mexpress)))
    (row-begin "<mrow>")
    (p-op (getsymbol op))
    (mPr_engine oprnd op rop)
    (row-end "</mrow>")))
;;      this function takes care the quotient function or "/" sign
;;
(defun mPr-quotient (mexpress)
  (row-begin "<mfrac><mrow>")
  (mPr_engine (cadr mexpress) 'mparen 'mparen)
  (row-end "</mrow>")
  (row-begin "<mrow>")
  (mPr_engine (caddr mexpress) 'mparen 'mparen)
  (row-end "</mrow></mfrac>"))

(defun mPr-rat (mexpress) (mPr-quotient mexpress))

;;      this function handles binomial coefficients
;;
(defun mPr-binomial(mexpress)
    (row-begin "<mrow><mfenced open='(' close=')'><mfrac linethickness='0'><mrow>")
       (mPr_engine (cadr mexpress) 'mparen 'mparen)
       (tprinc "</mrow><mrow>")
       (mPr_engine (caddr mexpress) 'mparen 'mparen)
    (row-end  "</mrow></mfrac></mfenced></mrow>")
)

;;      this function handles sqrt
;;
(defun mPr-sqrt (mexpress)
  (tprinc "<msqrt>")
  (mPr_engine (cadr mexpress) 'mparen 'mparen)
  (tprinc "</msqrt>"))
    
;;      This function taks care both sum(exp,ind,lo,hi) and 
;; product(exp,ind,lo,hi)
;;ind
;;low
;; hi
;;exp
(defun mPr-sumprod (mexpress)
  (row-begin "<mrow>")(tprinc "<munderover>")
  (p-op (getsymbol (caar mexpress)))
  (row-begin "<mrow>")
  (mPr_engine (caddr mexpress) 'mparen 'mequal)
  (tprinc "<mo>=</mo>")
  (mPr_engine (meval 
    (list '($substitute) '((mminus) $inf) '$minf (cadddr mexpress)))
    'mequal 'mparen)
  (row-end "</mrow>")
  (mPr_engine  (meval
    (list '($substitute) '((mminus) $inf) '$minf (car (cddddr mexpress))))
    'mparen 'mparen)
  (tprinc "</munderover>")
  (mPr_engine (cadr mexpress) 'mparen rop)
  (row-end "</mrow>"))
;;      mPr-times a function handle multiplication
(defun mPr-times (mexpress)
  (let ((lop 'mtimes) (rop 'mtimes)) (mPr-infix mexpress)))

;;;;;;; Operators

(setup '(mlist (mPrprocess mPr-list)))

(setup '(mplus (mPrprocess mPr-plus) (mPr-lbp 100) (mPr-rbp 100)
               (chchr "+")))

(setup '(mminus (mPrprocess mPr-prefix) (mPr-lbp 100) (mPr-rbp 100)
                (chchr "-")))

(setup '(mquote (mPrprocess mPr-prefix) (mPr-rbp 201) (chchr "'")))

(setup '(mand (mPrprocess mPr-infix) (mPr-lbp 60) (mPr-rbp 60)
              (chchr "and")))

(setup '(mor (mPrprocess mPr-infix) (mPr-lbp 50) (mPr-rbp 50)
             (chchr "or")))

(setup '(mnot (mPrprocess mPr-prefix) (mPr-rbp 70) (chchr "&not;")))

(setup '(mgreaterp (mPrprocess mPr-infix) (mPr-lbp 80) (mPr-rbp 80)
            (chchr "&gt;")))

(setup '(mgeqp (mPrprocess mPr-infix) (mPr-lbp 80) (mPr-rbp 80)
               (chchr "&ge;")))


(setup '(mnotequal (mPrprocess mPr-infix) (mPr-lbp 80) (mPr-rbp 80)
            (chchr "&NotEqual;")))

(setup '(mleqp (mPrprocess mPr-infix) (mPr-lbp 80) (mPr-rbp 80)
               (chchr "&le;")))

(setup '(mlessp (mPrprocess mPr-infix) (mPr-lbp 80) (mPr-rbp 80)
                (chchr "&lt;")))

(setup '(msetq (mPrprocess mPr-infix) (mPr-lbp 180) (mPr-rbp 20)
               (chchr "&Assign;")))

(setup '(mset (mPrprocess mPr-infix) (mPr-lbp 180) (mPr-rbp 20)
               (chchr "&Assign;")))  ;;; This is not math

(setup '(mdefine (mPrprocess mPr-infix) (mPr-lbp 180) (mPr-rbp 20)
                 (chchr ":=")))

(setup '(mfactorial (mPrprocess mPr-postfix) (mPr-lbp 160) (chchr "!")))

(setup '(mabs (mPrprocess mPr-abs)))

(setup '(%abs (mPrprocess mPr-abs)))

(setup '(mnctimes (mPrprocess mPr-infix) (mPr-lbp 110) (mPr-rbp 109)
            (chchr "&CenterDot;")))

(setup '(marrow (mPrprocess mPr-infix) (mPr-lbp 180) (mPr-rbp 20)
                (chchr "&RightArrow;")))

(setup '(mrarrow (mPrprocess mPr-prefix) (mPr-lbp 180) (mPr-rbp 20)
                (chchr "&RightArrow;")))

(setup '(mdif (mPrprocess mPr-infix) (mPr-lbp 100) (mPr-rbp 100)
              (chchr "-")))

(setup '(mtimes (mPrprocess mPr-times) (mPr-lbp 120) (mPr-rbp 120)
                (chchr "&InvisibleTimes;")))

(setup '(mdottimes (mPrprocess mPr-infix) (mPr-lbp 120) (mPr-rbp 120)
            (chchr "&CenterDot;")))

(setup '(mexpt (mPrprocess mPr-expt) (mPr-lbp 140) (mPr-rbp 139)))

(setup '(mncexpt (mPrprocess mPr-expt) (mPr-lbp 135) (mPr-rbp 134)))

(setup '(%at (mPrprocess mPr-at)))

(setup '($at (mPrprocess mPr-at)))

(setup '($det (mPrprocess mPr-det)))

(setup '(%determinant (mPrprocess mPr-det)))

(setup '($binomial (mPrprocess mPr-binomial)))

(setup '(%binomial (mPrprocess mPr-binomial)))

(setup '(%sum (mPrprocess mPr-sumprod) (chchr "&Sum;")))

(setup '($sum (mPrprocess mPr-sumprod) (chchr "&Sum;")))

(setup '($product (mPrprocess mPr-sumprod) (chchr "&Prod;")))

(setup '(%product (mPrprocess mPr-sumprod) (chchr "&Prod;")))

(setup '($integrate (mPrprocess mPr-integrate) (chchr "&Integral;")))

(setup '(%integrate (mPrprocess mPr-integrate) (chchr "&Integral;")))

(setup '($diff (mPrprocess mPr-diff) (chchr "<mo>&dd;</mo>")))

(setup '(%derivative (mPrprocess mPr-diff) (chchr "<mo>&dd;</mo>")))

(setup '($limit (mPrprocess mPr-limit)))

(setup '(%limit (mPrprocess mPr-limit)))

(setup '($sqrt (mPrprocess mPr-sqrt) (chchr "&Sqrt;")))

(setup '(%sqrt (mPrprocess mPr-sqrt) (chchr "&Sqrt;")))

(setup '(%binomial (mPrprocess mPr-binomial)))

(setup '(mquotient (mPrprocess mPr-quotient) (mPr-lbp 122)
            (mPr-rbp 123) (chchr "<mo>/</mo>"))) 

(setup '(rat (mPrprocess mPr-rat) (mPr-lbp 120) (mPr-rbp 121)))

(setup '(mconc (mPrprocess mPr-infix) (chchr " ")))

(setup '(mparen (chchr " "))) 

(setup '(mbrak (chchr " "))) 

(setup '(mequal (mPrprocess mPr-infix) (mPr-lbp 80) (mPr-rbp 80)
                (chchr "=")))

;;(setup '(mmsubs (mPrprocess mPr-mmsubs) (chchr "&")))

(setup '(mqapply (mPrprocess mPr-mqapply))) 

(setup '(mmfunct (mPrprocess mPr-funct))) 

(setup '($matrix (mPrprocess mPr-matrix)))

(setup '($%pi (chchr "&pi;")))

(setup '($%e (chchr "&ee;")))

(setup '($%gamma (chchr "&gamma;")))

(setup '($%phi (chchr "&phi;")))

(setup '(& (chchr "&amp;")))

(setup '(% (chchr "%")))

(setup '($ (chchr "$")))

(setup '(_ (chchr "_")))

(setup '($minus (chchr "-")))

(setup '($plus (chchr "+")))
;;

(setup '(mprog (chchr "block")))

(setup '($$block (chchr "block")))

(setup '($$boldif (chchr "if")))

(setup '($$boldthen (chchr "then")))

(setup '($$boldelse (chchr "else")))


;;;; routines to access these fields

;;      The following are databases for special characters

(setf (get '$inf 'chchr) '"&infin;")
;;;(setf (get '$minf 'chchr) '"<mo>-</mo>&infin;")

;;      lower case greek database

(setf (get '$alpha 'chchr) '"&alpha;")
(setf (get '%alpha 'chchr) '"&alpha;")
(setf (get '$beta 'chchr) '"&beta;")
(setf (get '$gamma 'chchr) '"&gamma;")
(setf (get '%gamma 'chchr) '"&gamma;")
(setf (get '$delta 'chchr) '"&delta;")
(setf (get '$epsilon 'chchr) '"&epsilon;")
(setf (get '$varepsilon 'chchr) '"&varepsilon;")
(setf (get '$zeta 'chchr) '"&zeta;")
(setf (get '$eta 'chchr) '"&eta;")
(setf (get '$theta 'chchr) '"&theta;")
(setf (get '$vartheta 'chchr) '"&vartheta;")
(setf (get '$iota 'chchr) '"&iota;")
(setf (get '$kappa 'chchr) '"&kappa;")
(setf (get '$lambda 'chchr) '"&lambda;")
(setf (get 'lambda 'chchr) '"&lambda;")
(setf (get '$mu 'chchr) '"&mu;")
(setf (get '$nu 'chchr) '"&nu;")
(setf (get '$xi 'chchr) '"&xi;")
(setf (get '$pi 'chchr) '"&pi;")
(setf (get '$varpi 'chchr) '"&varpi;")
(setf (get '$rho 'chchr) '"&rho;")
(setf (get '$varrho 'chchr) '"&varrho;")
(setf (get '$sigma 'chchr) '"&sigma;")
(setf (get '$varsigma 'chchr) '"&varsigma;")
(setf (get '$tau 'chchr) '"&tau;")
(setf (get '$upsilon 'chchr) '"&upsilon;")
(setf (get '$phi 'chchr) '"&phi;")
(setf (get '$varphi 'chchr) '"&varphi;")
(setf (get '$chi 'chchr) '"&chi;")
(setf (get '$psi 'chchr) '"&psi;")
(setf (get '$omega 'chchr) '"&omega;")

;;      Greek Upper Case Database

(setf (get '|$Alpha| 'chchr) '"&Alpha;")
(setf (get '|$Gamma| 'chchr) '"&Gamma;")
(setf (get '|$Delta| 'chchr) '"&Delta;")
(setf (get '|$Theta| 'chchr) '"&Theta;")
(setf (get '|$Lambda| 'chchr) '"&Lambda;")
(setf (get '|$Xi| 'chchr) '"&Xi;")
(setf (get '|$Pi| 'chchr) '"&Pi;")
(setf (get '|$Sigma| 'chchr) '"&Sigma;")
(setf (get '|$Upsilon| 'chchr) '"&Upsilon;")
(setf (get '|$Phi| 'chchr) '"&Phi;")
(setf (get '|$Psi| 'chchr) '"&Psi;")
(setf (get '|$Omega| 'chchr) '"&Omega;")
(setf (get '|$Re| 'chchr) '"&Re;")
(setf (get '|$Im| 'chchr) '"&Im;")

;;      Miscellaneous symbols

(setf (get '$aleph 'chchr) '"&aleph;")
(setf (get '$hbar 'chchr) '"&hbar;")
(setf (get '$%i 'chchr) '"&ii;")
(setf (get '$%j 'chchr) '"&ij")
(setf (get '$ell 'chchr) '"&ell;")
(setf (get '$wp 'chchr) '"&wp;")
(setf (get '$mho 'chchr) '"&mho;")
(setf (get '$infty 'chchr) '"&infty;")
(setf (get '$nabla 'chchr) '"&nabla;")
(setf (get '$partial 'chchr) '"&PartialD;")
(setf (get '$triangle 'chchr) '"&triangle;")


(setup '(%sin (mPrprocess mPr-function) (mPr-rbp 110) (chchr"sin")))
(setup '(%cos (mPrprocess mPr-function) (mPr-rbp 110) (chchr"cos")))
(setup '(%tan (mPrprocess mPr-function) (mPr-rbp 110) (chchr"tan")))
(setup '(%cot (mPrprocess mPr-function) (mPr-rbp 110) (chchr"cot")))
(setup '(%sec (mPrprocess mPr-function) (mPr-rbp 110) (chchr"sec")))
(setup '(%csc (mPrprocess mPr-function) (mPr-rbp 110) (chchr"csc")))

(setup '(%asin (mPrprocess mPr-function) (mPr-rbp 110) (chchr"arcsin")))
(setup '(%acos (mPrprocess mPr-function) (mPr-rbp 110) (chchr"arccos")))
(setup '(%atan (mPrprocess mPr-function) (mPr-rbp 110) (chchr"arctan")))
(setup '(%acot (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acot")))
(setup '(%asec (mPrprocess mPr-function) (mPr-rbp 110) (chchr"asec")))
(setup '(%acsc (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acsc")))
(setup '(%sinh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"sinh")))
(setup '(%cosh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"cosh")))
(setup '(%tanh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"tanh")))
(setup '(%coth (mPrprocess mPr-function) (mPr-rbp 110) (chchr"coth")))
(setup '(%sech (mPrprocess mPr-function) (mPr-rbp 110) (chchr"sec")))
(setup '(%csch (mPrprocess mPr-function) (mPr-rbp 110) (chchr"csch")))


(setup '(%asinh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"asinh")))
(setup '(%acosh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acosh")))
(setup '(%atanh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"atanh")))
(setup '(%acoth (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acoth")))
(setup '(%asech (mPrprocess mPr-function) (mPr-rbp 110) (chchr"asec")))
(setup '(%acsch (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acsch")))

(setup '(%ln (mPrprocess mPr-function) (mPr-rbp 110) (chchr"ln")))
(setup '(%log (mPrprocess mPr-function) (mPr-rbp 110) (chchr"log")))

(setup '($sin (mPrprocess mPr-function) (mPr-rbp 110) (chchr"sin")))
(setup '($cos (mPrprocess mPr-function) (mPr-rbp 110) (chchr"cos")))
(setup '($tan (mPrprocess mPr-function) (mPr-rbp 110) (chchr"tan")))
(setup '($cot (mPrprocess mPr-function) (mPr-rbp 110) (chchr"cot")))
(setup '($sec (mPrprocess mPr-function) (mPr-rbp 110) (chchr"sec")))
(setup '($csc (mPrprocess mPr-function) (mPr-rbp 110) (chchr"csc")))

(setup '($asin (mPrprocess mPr-function) (mPr-rbp 110) (chchr"arcsin")))
(setup '($acos (mPrprocess mPr-function) (mPr-rbp 110) (chchr"arccos")))
(setup '($atan (mPrprocess mPr-function) (mPr-rbp 110) (chchr"arctan")))
(setup '($acot (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acot")))
(setup '($asec (mPrprocess mPr-function) (mPr-rbp 110) (chchr"asec")))
(setup '($acsc (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acsc")))

(setup '($sinh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"sinh")))
(setup '($cosh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"cosh")))
(setup '($tanh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"tanh")))
(setup '($coth (mPrprocess mPr-function) (mPr-rbp 110) (chchr"coth")))
(setup '($sech (mPrprocess mPr-function) (mPr-rbp 110) (chchr"sec")))
(setup '($csch (mPrprocess mPr-function) (mPr-rbp 110) (chchr"csch")))

(setup '($asinh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"asinh")))
(setup '($acosh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acosh")))
(setup '($atanh (mPrprocess mPr-function) (mPr-rbp 110) (chchr"atanh")))
(setup '($acoth (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acoth")))
(setup '($asech (mPrprocess mPr-function) (mPr-rbp 110) (chchr"asec")))
(setup '($acsch (mPrprocess mPr-function) (mPr-rbp 110) (chchr"acsch")))
(setup '($ln (mPrprocess mPr-function) (mPr-rbp 110) (chchr"ln")))
(setup '($log (mPrprocess mPr-function) (mPr-rbp 110) (chchr"log")))
;;
;;
;;      set the preference feature
;;
($lessparen)
(setq casep nil)                ;set to distinguish a capital or lower case
(setq $mPrworksheet nil)        ;set TeX worksheet mode false
(setq $lamPrworksheet nil)        ;set LaTeX worksheet mode false
(setq $mPrlabelleft nil)        ;set Tex or LaTeX left Labeling mode false
(setq $mPrdisplaytype t)        ;set default for TeX or LaTeX in display type
(setq $mPrevaluate t)   ;set default for evaluating macsyma expression
(setq $mPrautolabel nil) ;set autolabel mode off, can be set to be integer
(setq $lamPrautolabel nil)      ;set LaTeX autolabel mode false




