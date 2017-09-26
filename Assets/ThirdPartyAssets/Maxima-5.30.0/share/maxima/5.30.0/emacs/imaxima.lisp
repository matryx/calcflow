;; Copyright (C) 2001, 2002, 2003, 2004 Jesper Harder
;; Copyright (C) 2007, 2008 Yasuaki Honda
;;
;; Plotting support section of this file is the copy of the same
;; section of wxmathml.lisp with very small modification. The file
;; wxmathml.lisp is a part of the distribution of wxMaxima.

;; Author: Jesper Harder <harder@ifa.au.dk>
;; Created: 14 Nov 2001
;; Version: 1.0b
;; Keywords: maxima
;; $Id: imaxima.lisp,v 1.8 2011-01-05 22:49:31 riotorto Exp $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;; History:
;;; Putting prefix package name to the variable does cause
;;; error when the file is loaded into older clisp, e.g. 2.29.
;;; I changed it so that it checks the existence of the symbol
;;; in the custom package prior to accessing it.
;;; by yasuaki honda 2006/02/26
;;;
;;; When imaxima-setup bug in imaxima.el was fixed,
;;; it became clear that the following code does not work
;;; when imaxima.lisp is loaded by:
;;;     %i1 load("/xxx/imaxima.lisp");
;;; Putting prefix package name to the variable solves the
;;; issue.
;;; by yasuaki honda

;;;
;;; There is a report that some Linux provides Maxima with GCL
;;; which does not support handler-bind. The macro
;;; style-warning-suppressor
;;; is introduced to check if handler-bind is defined or not.
;;;
;;; by yasuaki honda 2007/06/10
;;;

(in-package :maxima)

(defvar *old-tex-atom* #'tex-atom)
(defvar *windows-OS* (string= *autoconf-win32* "true"))
(defmvar $wxplot_size '((mlist simp) 400 250))
(defmvar $wxplot_old_gnuplot nil)
(defvar *image-counter* 0)


;;; Following function wx-gnuplot-installed-p and the macro
;;; wx-gnuplot-installation-check should be in plotting section
;;; later in this file. However, because they must be in the
;;; real toplevel, they are moved here.
;;; yasuaki honda
(defun wx-gnuplot-installed-p ()
  #+gcl
  (cond ((member :linux *features*)
	 (let* ((tmp-stream (open "| which gnuplot" :direction :input))
		(result (read-line tmp-stream nil :eof)))
	   (if (eql result :eof) nil t)))
	((and (member :mingw32 *features*)
	      (probe-file "c:\\Windows\\System32\\where.exe"))
	 (let* ((tmp-stream (open "| where wgnuplot" :direction :input))
		(result (read-line tmp-stream nil :eof)))
	   (if (eql result :eof) nil t)))
	(t t))
  #-gcl
  ;; The function check-gnuplot-process is defined in
  ;; maxima/src/plot.lisp since at least 5.12.0.
  (handler-case (progn (check-gnuplot-process) t)
		(error () nil)))

(defun wx-gnuplot-installation-check ()
  (if (not (wx-gnuplot-installed-p))
      (merror (format t "Gnuplot error: Gnuplot is not installed,
nor Gnuplot is not recognized by maxima"))))

(defmacro style-warning-suppressor (&rest body)
  (if (member :clisp *features*)
      (setq body (cons
		  '(let ((scr (find-symbol "*SUPPRESS-CHECK-REDEFINITION*" :CUSTOM)))
		     (if scr (set scr t)))
		  body)))
  (if (macro-function 'handler-bind)
      `(handler-bind ((style-warning #'muffle-warning))
		     ,@body)
    `(progn ,@body)))

(style-warning-suppressor

(declare-top
	 (special lop rop $gcprint $inchar *autoconf-version*)
	 (*expr tex-lbp tex-rbp))

;;;
;;; Very unfortunately, the following code does not work in
;;; SBCL.
;;; by yasuaki honda
#-sbcl
(unless (fboundp 'maxima::print-invert-case)
  (defun print-invert-case (obj)
    (princ-to-string obj)))

(defun print-case-sensitive (obj)
  (if obj
      (print-invert-case obj)
    nil))

(defun diff-symbol () '$d)

(defun memq (elem seq)
  #+(or cmu scl) (declare (inline member))
  (member elem seq :test #'eq))

(defun main-prompt ()
  (format () (concatenate 'string (string (code-char 3)) "(~A~D) " (string (code-char 4)))
    (stripdollar (print-case-sensitive $inchar)) $linenum))

(defun break-dbm-loop (at)
  (let* (
	 (*quit-tags* (cons (cons *break-level* *quit-tag*) *quit-tags*))
	 (*break-level* (if (not at) *break-level* (cons t *break-level*)))
	 (*quit-tag* (cons nil nil))
	 (*break-env* *break-env*)
	 (*mread-prompt* "")
	 (*diff-bindlist* nil)
	 (*diff-mspeclist* nil)
	 val
	 )
    (declare (special *mread-prompt* ))
    (and (consp at) (set-env at))
    (cond ((null at)
	   (break-frame 0 nil)))
    (catch 'step-continue
      (catch *quit-tag*
	(unwind-protect
	    (do () (())
		(format *debug-io*
			(concatenate 'string
				     (string (code-char 3))
				     "~&~@[(~a:~a) ~]"
				     (string (code-char 4)))
		    (unless (stringp at) "dbm")
		    (length *quit-tags*))
		(setq val
		      (catch 'macsyma-quit
			(let ((res (dbm-read *debug-io*  nil *top-eof* t)))
			  (declare (special *mread-prompt*))
			  (cond ((and (consp res) (keywordp (car res)))
				 (let ((value (break-call (car res)
							  (cdr res) 'break-command)))
				   (cond ((eq value :resume) (return)))
				   ))
				(t
				 (setq $__ (nth 2 res))
				 (setq $% (meval* $__))
				 (setq $_ $__)
				 (displa $%)
				 ))
			  nil
			  )))
		(and (eql val 'top)
		     (throw-macsyma-top))
		      )
	 (restore-bindings)
	)))))

(setq $display2d '$imaxima)

;; TeX-printing
;; (c) copyright 1987, Richard J. Fateman
;; Small changes for interfacing with TeXmacs: Andrey Grozin, 2001
;; Yet more small changes for interfacing with imaxima: Jesper Harder 2001

;; (defun tex (... is removed

#|
(defun tex-atom (x l r) ;; atoms: note: can we lose by leaving out {}s ?
  (append l 
	  (list (cond ((numberp x) (texnumformat x))
		      ((and (symbolp x) (or (get x 'texword) (get (get x 'reversealias) 'texword))))
		      ;; This is different from the one in mactex.lisp
                      ((mstringp x) (texstring x))
                      ((characterp x) (texchar x))
		      (t (tex-stripdollar (or (get x 'reversealias) x)))))
	  
	  r))
|#

(defun tex-atom (x l r &aux other-case) ;; atoms: note: can we lose by leaving out {}s ?
  (let ((result (append l
			(list (cond ((mstringp x) (texstring x))
				    ((characterp x) (texchar x))
				    (t (setq other-case t))))
			r)))
    (if other-case
	(funcall *old-tex-atom* x l r)
      result)))

(defun texstring (x)
  (let ((sym-name
	 (if (symbolp x)
	     (print-case-sensitive x)
	   x)))
    (cond ((equal sym-name "") "")
	  ((eql (elt sym-name 0) #\\) sym-name)
	  ((memq (elt sym-name 0) '(#\$ #\&))
	   (setq sym-name (subseq sym-name 1))
	   (concatenate 'string "\\verb|   " (verb-quote sym-name) "|"))
	  (t (concatenate 'string "\\verb|" (verb-quote sym-name) "|")))))

(defun verb-quote (str)
  (let ((var "") (charlist
		  '((#\Newline . "| \\\\ \\verb| "))))
    (dotimes (i (length str))
      (let ((chari (elt str i)))
	(setq var (concatenate 'string var 
			       (or (cdr (assoc chari charlist :test #'eql))
				   (string chari))))))
  var))


(defun texchar (x)
  (if (eql x #\|) "\\verb/|/"
    (concatenate 'string "\\verb|" (string x) "|")))

(defun myquote (str)
  (let ((var "") (charlist
		  '((#\{ . "\\left\\{\\right.")
		    (#\} . "\\left\\}\\right.")
		    (#\space . "\\ ")
		    (#\Newline . "} \\\\ \\mathrm{ ")
		    (#\# . "\\#")
		    (#\$ . "\\$")
		    (#\% . "\\%")
		    (#\& . "\\&")
		    (#\_ . "\\_"))))
    (dotimes (i (length str))
      (let ((chari (elt str i)))
	(setq var (concatenate 'string var 
			       (or (cdr (assoc chari charlist :test #'eql))
				   (string chari))))))
  var))

(defun tex-stripdollar (sym)
  (or (symbolp sym) (return-from tex-stripdollar sym))
  (let* ((name (print-case-sensitive sym))
      (pname (if (memq (elt name 0) '(#\$ #\&)) (subseq name 1) name))
      (l (length pname)))
    (cond
     ((eql l 1) (myquote pname))
     (t (concatenate 'string "\\mathrm{" (myquote pname) "}")))))

;; (defun strcat (... is removed

;; 10/14/87 RJF  convert 1.2e20 to 1.2 \cdot 10^{20}
;; 03/30/01 RLT  make that 1.2 \times 10^{20}
;; (defun texnumformat(atom) is removed

;; (defun tex-paren (x l r)  is removed

;;;
;;; The definition of tex-array is modified to fix bug #30, reported by Thomas Weidner.
;;; The following definition is provided by Thomas. 
;;; Dec.6, 2006
;;;

(defun tex-array (x l r)
 (let ((f))
      (if (eq 'mqapply (caar x))
          (setq f (cadr x)
                x (cdr x))
          (setq f (caar x)))
      (if (and (atom (cadr x)) (atom f))
          ;; subscript is an atom -- don't use \isubscript
          (progn
            (setq l (tex f l nil lop 'mfunction)
                  r (nconc (tex-list (cdr x) nil (list "}") ",") r))
            (nconc l (list "_{") r))
        (progn
          (setq l (tex f (append l (list "\\isubscript{"))  nil lop 'mfunction)
                r (nconc (tex-list (cdr x) nil (list "}") ",") r))
          (nconc  l (list "}{") r )))))

;; set up a list , separated by symbols (, * ...)  and then tack on the
;; ending item (e.g. "]" or perhaps ")"

(defun tex-list (x l r sym)
  (if (null x) r
      (do ((nl))
	  ((null (cdr x))
	   (setq nl (nconc nl (tex (car x)  l r 'mparen 'mparen)))
	   nl)
;;	  (setq nl (nconc nl (tex (car x)  l (list sym) 'mparen 'mparen))
	  (setq nl (nconc nl (tex (car x)  l (list (concatenate 'string sym "\\linebreak[0]")) 'mparen 'mparen))
		  x (cdr x)
		  l nil))))

;; (defun tex-prefix (x l r) is removed

;; (defun tex-infix (x l r) is removed
  
;; (defun tex-postfix (x l r) is removed

;; (defun tex-nary (x l r) is removed
#|
(defun tex-nary (x l r)
  (let* ((op (caar x)) (sym (texsym op)) (y (cdr x)) (ext-lop lop) (ext-rop rop))
    (cond ((null y)       (tex-function x l r t)) ; this should not happen
          ((null (cdr y)) (tex-function x l r t)) ; this should not happen, too
          (t (do ((nl) (lop ext-lop op) (rop op (if (null (cdr y)) ext-rop op)))
                 ((null (cdr y)) (setq nl (nconc nl (tex (car y)  l r lop rop))) nl)
	         (setq nl (nconc nl (tex (car y)  l (list sym)   lop rop))
		       y (cdr y) 
		       l nil))))))
|#
;; (defun tex-nofix (x l r) is removed

;; (defun tex-matchfix (x l r) is removed

;; (defun texsym (x) is removed

;; (defun texword (x) is removed

;; (defprop bigfloat tex-bigfloat tex) is removed

;;;
;;; Fixed to treat big float correctly.
;;;
(defun tex-bigfloat (x l r) (tex-list (fpformat x) l r nil))

;;absolute value
(defprop $%phi "\\phi" texword) ;; yhonda

;; reported conjugate treatment in imaxima be fixed.
(defprop $conjugate ("^{\\star}") texsym)

(defprop mquote 201. tex-rbp)

(defprop msetq 180. tex-rbp)
(defprop msetq 20. tex-rbp)

(defprop mset 180. tex-lbp)
(defprop mset 20. tex-rbp)

(defprop mdefine 180. tex-lbp)
(defprop mdefine 20. tex-rbp)

(defprop mdefmacro 180. tex-lbp)
(defprop mdefmacro 20. tex-rbp)

(defprop marrow 25 tex-lbp)
(defprop marrow 25 tex-rbp)

(defprop mfactorial 160. tex-lbp)

(defprop mexpt 140. tex-lbp)
(defprop mexpt 139. tex-rbp)


(defprop mncexpt 135. tex-lbp)
(defprop mncexpt 134. tex-rbp)

(defprop mnctimes 110. tex-lbp)
(defprop mnctimes 109. tex-rbp)

;;(defprop mtimes tex-nary tex)
;;(defprop mtimes "\\*" texsym)
(defprop mtimes 120. tex-lbp)
(defprop mtimes 120. tex-rbp)

(defprop %sqrt tex-sqrt tex)

(defun tex-sqrt(x l r)
;; format as \\sqrt { } assuming implicit parens for sqr grouping
   (tex (cadr x) (append l  '("\\isqrt{")) (append '("}") r) 'mparen 'mparen))

(defprop mquotient 122. tex-lbp) ;;dunno about this
(defprop mquotient 123. tex-rbp) 

(defun tex-mquotient (x l r)
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (cond ((and (atom (cadr x)) (atom (caddr x)))
	 ;; both denom and numerator are atoms
	 (setq l (tex (cadr x) (append l '("\\frac{")) nil nil nil) ;;fixme
	       r (tex (caddr x) (list "}{") (append '("}")r) 'mparen 'mparen)))
	((atom (cadr x))
	 ;; numerator is an atom
	 (setq l (tex (cadr x) (append l '("\\ifracd{")) nil 'mparen 'mparen)
	       r (tex (caddr x) (list "}{") (append '("}")r) 'mparen 'mparen)))
	((atom (caddr x))
	 ;; denom is an atom
	 (setq l (tex (cadr x) (append l '("\\ifracn{")) nil 'mparen 'mparen)
	       r (tex (caddr x) (list "}{") (append '("}")r) 'mparen 'mparen)))
	(t
	 ;; neither are atoms
	 (setq l (tex (cadr x) (append l '("\\ifrac{")) nil 'mparen 'mparen)
	       r (tex (caddr x) (list "}{") (append '("}")r) 'mparen 'mparen))))
  (append l r))

;; easily extended to union, intersect, otherops

;; (defun tex-limit(x l r) is removed. mactex.lisp version considers direction.

;;binomial coefficients

(defprop %binomial tex-choose tex)

;; (defun tex-choose (x l r) is removed

(defprop rat 120. tex-lbp)
(defprop rat 121. tex-rbp)

(defprop mplus 100. tex-lbp)
(defprop mplus 100. tex-rbp)

;; (defun tex-mplus (x l r) is removed

(defprop mminus 100. tex-rbp)
(defprop mminus 100. tex-lbp)

(defprop mequal 80. tex-lbp)
(defprop mequal 80. tex-rbp)

(defprop mnotequal 80. tex-lbp)
(defprop mnotequal 80. tex-rbp)

(defprop mgreaterp 80. tex-lbp)
(defprop mgreaterp 80. tex-rbp)

(defprop mgeqp 80. tex-lbp)
(defprop mgeqp 80. tex-rbp)

(defprop mlessp 80. tex-lbp)
(defprop mlessp 80. tex-rbp)

(defprop mleqp 80. tex-lbp)
(defprop mleqp 80. tex-rbp)

(defprop mnot 70. tex-rbp)

(defprop mand 80. tex-lbp)
(defprop mand 80. tex-rbp)

(defprop mor 50. tex-lbp)
(defprop mor 50. tex-rbp)

;; make sin(x) display as sin x , but sin(x+y) as sin(x+y)
;; etc

(mapc #'tex-setup 
  '( 
     (%acot "\\operatorname{arccot}")
     (%asec "\\operatorname{arcsec}")
     (%acsc "\\operatorname{arccsc}")
     (%sech "\\operatorname{sech}")          
     (%csch "\\operatorname{csch}")
     (%asinh "\\operatorname{arcsinh}")
     (%acosh "\\operatorname{arccosh}")
     (%atanh "\\operatorname{arctanh}")
     (%acoth "\\operatorname{arccoth}")
     (%asech "\\operatorname{arcsech}")
     (%acsch "\\operatorname{arccsch}")
     )) ;; etc

(defprop mcond 25. tex-lbp)
(defprop mcond 25. tex-rbp)
(defprop %derivative tex-derivative tex)

(defprop mdo 30. tex-lbp)
(defprop mdo 30. tex-rbp)
(defprop mdoin 30. tex-rbp)

;; these aren't quite right


;; Undone and trickier:
;; handle reserved symbols stuff, just in case someone
;; has a macsyma variable named (yuck!!) \over  or has a name with 
;; {} in it.
;; Maybe do some special hacking for standard notations for 
;; hypergeometric fns, alternative summation notations  0<=n<=inf, etc.

;;Undone and really pretty hard: line breaking


(defun tex-mtext (x l r) (tex-list (cdr x) l r ""))

(defun tex-mlabel (x l r)
  (tex (caddr x)
    (append l
      (if (cadr x)
	  (list (format nil (concatenate 'string (string (code-char 23))
					 "~A"
					 (string (code-char 23)))
			(myquote (print-case-sensitive (stripdollar (cadr x))))))
        nil))
    r 'mparen 'mparen))

(defun tex-spaceout (x l r)
  (append l (list "\\verb|" (make-string (cadr x) :initial-element #\space) "|") r))

; jh: verb & mbox

(defun latex (x)
;  (princ x)  ;; uncomment to debug.
  (if (and (listp x) (car x) (listp (car x)) (caar x)
	   (equal (caar x) 'mlabel)
	   (cdr x)
	   (cadr x)
	   (input-label-p (cadr x)))
      (let (($display2d nil))
	(declare (special $display2d))
	(displa x)
	(return-from latex)))
  (fresh-line)
  (mapc #'princ
	(if (and (listp x) (cdr x) (stringp (cadr x))
		 (equal (string-right-trim '(#\Space) (cadr x)) "Is"))
	    (tex x (list (string (code-char 21)))
		   (list (string (code-char 22))) 'mparen 'mparen)
	  (tex x (list (string (code-char 2)))
	         (list (string (code-char 5))) 'mparen 'mparen))))

(defun input-label-p (label)
  (if (symbolp label)
      (let ((name (symbol-name label)))
	(and (> (length name) 3)
	     (string= "$%I" (subseq name 0 3))))))

(let ((old-displa (symbol-function 'displa)))
  (defun displa (form)
    (if (eq $display2d '$imaxima)
        (latex form)
      (funcall old-displa form))))

(defun ask-prop (object property fun-or-number)
  (if fun-or-number (setq fun-or-number (list '| | fun-or-number)))
;;; Asks the user a question about the property of an object.
;;; Returns only $yes, $no or $unknown.
  (if (symbolp property)
      (setq property (print-case-sensitive property)))
  (do ((end-flag) (answer))
      (end-flag (cond ((memq answer '($yes |$Y| |$y|)) '$yes)
		      ((memq answer '($no |$N| |$n|)) '$no)
		      ((memq answer '($unknown $uk)) '$unknown)))
    (setq answer (retrieve
		  `((mtext) "Is  " ,object 
		    ,(if (member (getcharn property 1)
				 '(#\a #\e #\i #\o #\u)
				 :test #'char-equal)
			 '"  an "
			 '"  a ")
		    ,property ,@fun-or-number "?")
		  nil))
    (cond 
      ((memq answer '($yes |$Y| |$y| |$N| |$n| $no $unknown $uk))
       (setq end-flag t))
      (t (mtell
	  "~%Acceptable answers are Yes, Y, No, N, Unknown, Uk~%")))))

;;
;; Plotting support
;;
		       
(defun wxxml-tag (x l r)
  (let ((name (cadr x))
	(tag (caddr x)))
    (append l (list (format nil "<~a>~a</~a>" tag name tag)) r)))


(defun wxplot-filename (&optional (suff t) &aux filename)
  (incf *image-counter*)
  (setq filename
	(plot-temp-file (if suff
			    (format nil "maxout_~d.eps" *image-counter*)
			  (format nil "maxout_~d" *image-counter*))))
  (if (probe-file filename)
      (delete-file filename))
  filename)

(defun $wxplot_preamble ()
  (let ((frmt (if $wxplot_old_gnuplot
		  "set terminal postscript picsize ~d ~d; set zeroaxis;"
		  "set terminal postscript size ~d,~d; set zeroaxis;")))
    (format nil frmt
	    ($first $wxplot_size)
	    ($second $wxplot_size))))

(defun $range (i j)
  (let ((x (gensym)))
    (mfuncall '$makelist x x i j)))

(defun $wxplot2d (&rest args)
  ;; if gnuplot is not installed, this will terminate the
  ;; further execution.
  (wx-gnuplot-installation-check)
  (let ((preamble ($wxplot_preamble))
	(system-preamble (get-plot-option-string '$gnuplot_preamble 2))
	(filename (wxplot-filename)))
    (if (length system-preamble)
	(setq preamble (format nil "~a; ~a" preamble system-preamble)))
    (dolist (arg args)
      (if (and (listp arg) (eql (cadr arg) '$gnuplot_preamble))
	  (setq preamble (format nil "~a; ~a"
				 preamble (caddr arg)))))
    (apply #'$plot2d `(,@args
		       ((mlist simp) $plot_format $gnuplot)
;;		       ((mlist simp) $gnuplot_preamble ,preamble)
		       ((mlist simp) $gnuplot_term $ps)
		       ((mlist simp) $gnuplot_out_file ,filename)))
    ($ldisp `((wxxmltag simp) ,filename "img")))
  "")

(defun $wxplot3d (&rest args)
  ;; if gnuplot is not installed, this will terminate the
  ;; further execution.
  (wx-gnuplot-installation-check)
  (let ((preamble ($wxplot_preamble))
	(system-preamble (get-plot-option-string '$gnuplot_preamble 2))
	(filename (wxplot-filename)))
    (if (length system-preamble)
	(setq preamble (format nil "~a; ~a" preamble system-preamble)))
    (dolist (arg args)
      (if (and (listp arg) (eql (cadr arg) '$gnuplot_preamble))
	  (setq preamble (format nil "~a; ~a"
				 preamble (caddr arg)))))
    (apply #'$plot3d `(,@args
		       ((mlist simp) $plot_format $gnuplot)
;;		       ((mlist simp) $gnuplot_preamble ,preamble)
		       ((mlist simp) $gnuplot_term $ps)
		       ((mlist simp) $gnuplot_out_file ,filename)))
    ($ldisp `((wxxmltag simp) ,filename "img")))
  "")

(defun $wxdraw2d (&rest args)
  (apply #'$wxdraw
	 (list (cons '($gr2d) args))))


(defun $wxdraw3d (&rest args)
  (apply #'$wxdraw
	 (list (cons '($gr3d) args))))

(defun $wxdraw (&rest args)
  ;; if gnuplot is not installed, this will terminate the
  ;; further execution.
  (wx-gnuplot-installation-check)
  (let* ((filename (wxplot-filename nil))
	 (*windows-OS* t)
	 res)
    (if (not (fboundp '$draw))
	($load '$draw))
    ;; Usually the following is used.
    ;;    (setq res (apply #'$draw
    ;; However, CMUCL warns that function $draw is not defined.
    ;; To suppress this warning, symbol-function is used to make
    ;; clear that the runtime definition is used rather than 
    ;; read time.
    (setq res (apply (symbol-function '$draw)
		     (append
		      `(
			((mequal simp) $terminal $eps_color)
                        ((mequal simp) $dimensions
                                       ((mlist simp)
                                        ;; convert points to 1/100 of cm
                                        ,(* 3.53 ($first $wxplot_size))
                                        ,(* 3.53 ($second $wxplot_size))))
			((mequal simp) $file_name ,filename))
		      args)))
    ($ldisp `((wxxmltag simp) ,(format nil "~a.eps" filename) "img"))
    res))

(defun $wximplicit_plot (&rest args)
  ;; if gnuplot is not installed, this will terminate the
  ;; further execution.
  (wx-gnuplot-installation-check)
  (let ((preamble ($wxplot_preamble))
	(system-preamble (get-plot-option-string '$gnuplot_preamble 2))
	(filename (wxplot-filename)))
    (if (not (fboundp '$implicit_plot))
	($load '$implicit_plot))
    (if (length system-preamble)
	(setq preamble (format nil "~a; ~a" preamble system-preamble)))
    (dolist (arg args)
      (if (and (listp arg) (eql (cadr arg) '$gnuplot_preamble))
	  (setq preamble (format nil "~a; ~a"
				 preamble (caddr arg)))))
    (apply (symbol-function '$implicit_plot)
	   `(,@args
	     ((mlist simp) $plot_format $gnuplot)
;;           ((mlist simp) $gnuplot_preamble ,preamble)
	     ((mlist simp) $gnuplot_term $ps)
	     ((mlist simp) $gnuplot_out_file ,filename)))
    ($ldisp `((wxxmltag simp) ,filename "img")))
  "")


(defun $wxcontour_plot (&rest args)
  ;; if gnuplot is not installed, this will terminate the
  ;; further execution.
  (wx-gnuplot-installation-check)
  (let ((preamble ($wxplot_preamble))
	(system-preamble (get-plot-option-string '$gnuplot_preamble 2))
	(filename (wxplot-filename)))
    (if (length system-preamble)
	(setq preamble (format nil "~a; ~a" preamble system-preamble)))
    (dolist (arg args)
      (if (and (listp arg) (eql (cadr arg) '$gnuplot_preamble))
	  (setq preamble (format nil "~a; ~a" preamble (caddr arg)))))
    (apply #'$contour_plot `(,@args
			     ((mlist simp) $plot_format $gnuplot)
;;			     ((mlist simp) $gnuplot_preamble ,preamble)
			     ((mlist simp) $gnuplot_term $ps)
			     ((mlist simp) $gnuplot_out_file ,filename)))

    ($ldisp `((wxxmltag simp) ,filename "img")))
  "")

) ;; This paran closes style-warning-suppressor.
