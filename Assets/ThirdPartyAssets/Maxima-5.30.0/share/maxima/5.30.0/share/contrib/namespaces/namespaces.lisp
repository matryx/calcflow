; namespaces.lisp -- user-level namespace system for Maxima
; by Robert Dodier, copyright 2006, released under terms of GNU General Public License.

; Known warts:
; * input labels appears as ($%i1) when namespace is something other than maxima
; * true and false parse to $TRUE and $FALSE, not T and NIL, when namespace is something other than maxima
;
; Functions:
; 
; * Functions to establish a namespace:
;
;   in_namespace(<s>)
;       -- makes the namespace named by symbol <s> the current namespace.
;          If a namespace named by <s> does not exist yet, a new namespace
;          is created and assigned to <s>.
;
;   export(<s_1>, ..., <s_n>)
;       -- marks symbols <s_1>, ..., <s_n> for export from current namespace;
;          has no other effect until import is called somewhere else.
;
;
; * Functions to refer to a namespace:
;
;   load_namespace(<s>)
;       -- pushes the current namespace onto a stack, calls `load(<s>)',
;          and then pops the stack.
;
;   require (<s_1>, ..., <s_n>)
;       -- for each symbol <s_k>, if <s_k> names an already-defined namespace,
;          then do nothing, otherwise, call load_namespace(<s_k>)
;
;   import(<ns_1>, ..., <ns_n>)
;       -- imports external symbols from namespaces <ns_1>, ..., <ns_n>.
;          An imported symbol hides any other symbol of the same name.
;          import does not attempt to load or otherwise define any undefined
;          namespace; if a namespace is not yet defined, import complains.
;
;   operator |
;       -- namespace resolution operator.
;          `foo | bar' returns symbol bar from namespace foo .
;          The left-hand side (foo in the example) is evaluated.
;
;   external_symbols(<ns>)
;       -- returns list of external symbols in namespace <ns>.
;
;   symbols(<ns>)
;       -- returns list of all symbols (excluding imported symbols)
;          in namespace <ns>.
;
;
; Variables:
;
;   namespaces
;       -- list of user-defined namespaces.
;
;   maxima
;       -- default namespace. All built-in functions and variables are exported.
;          All other namespaces import from this namespace.
;
;
; Example:

#|
    /* In foo.mac:
     * (put foo.mac in current working directory)
     */
    in_namespace (foo);
    export (f, g);
    my_constant : 1729;
    f(x) := my_constant * sin(x);
    g(n) := expand ((f(a) + n!)^n);
    h(z) := f(z) - g(2);

    /* In Maxima session: */
    load ("namespaces.lisp");
    maxima;
    external_symbols (maxima);
    load_namespace (foo);
    namespaces;
    foo;
    symbols (foo);
    external_symbols (foo);
    functions;
    values;
    my_constant : 1 - %pi;
    foo|my_constant;
    'foo|my_constant;
    ''%;
    h(x) := (1/2) * x^2;
    import (foo);
    dispfun (f, g, h, foo|h);
    f(%pi/4);
    g(3);
    h(u - v);
    foo|h(u - v);
    kill (foo|h);
    foo|h(u - v);
    h(u - v);

    in_namespace (aa);
    x_aa : y_aa + z_aa;
    f_aa (p, q) := (q - p)/x_aa;

    my_constant;
    maxima|my_constant;
    maxima|foo|my_constant;
    
    /* Following should redefine kill and save only in namespace aa.
     * However, because package $AA uses MAXIMA, $KILL and $SAVE resolve
     * to the corresponding symbols in MAXIMA.
     * I would like for $SAVE and $KILL to be interned in $AA (I think)
     * but I don't know a way to make that happen, and still have symbols
     * in MAXIMA accessible in $AA.
     */
    kill (a) := a + 1000000;
    save (a) := sin(a) + cos(a);

    in_namespace (bb);
    x_bb : sin(y_bb);
    f_bb (r) := exp(r) - 1;

    in_namespace (cc);
    x_cc : maxima|foo|my_constant - maxima|my_constant;
    f_cc (s, t) := x_cc * s / t;

    in_namespace (maxima);
    in_namespace ();
    
    trace (aa|f_aa, aa|bb|f_bb);

    aa|f_aa (xx, 17);

    aa|bb|f_bb (t - u);
|#

(in-package :maxima)

(setq $file_search_maxima `((mlist) ,@(cons "./###.mac" (cdr $file_search_maxima))))

(defvar $namespaces '((mlist)))
(setq $infolists (append $infolists '($namespaces)))
;!! ;; CONSTANT DECLARATION DOESN'T PREVENT REASSIGNMENT ... SEEMS LIKE A BUG
;!! (kind '$namespaces '$constant)

(defvar $maxima (find-package :maxima))
;!! ;; CONSTANT DECLARATION DOESN'T PREVENT REASSIGNMENT ... SEEMS LIKE A BUG
;!! (kind '$maxima '$constant)

(defun $symbols (p)
  (let ((symbols nil))
    (do-symbols (s (package-name p))
      (multiple-value-bind (s2 status) (find-symbol (symbol-name s) p)
        (declare (ignore s2))
        (if (or (eq status :internal) (eq status :external))
          (if (not (stringp s))
            (setq symbols (cons s symbols))))))
    ($sort (cons '(mlist) symbols))))

(defun $external_symbols (p)
  (let ((symbols nil))
    (do-external-symbols (s (package-name p))
      (if (not (stringp s))
        (setq symbols (cons s symbols))))
    ($sort (cons '(mlist) symbols))))

(defmspec $export (l)
  `((mlist) ,@(mapcar #'export (cdr l))))

(defvar *namespace-stack* nil)

(defmspec $load_namespace (form)
  (push *package* *namespace-stack*)
  (unwind-protect
       (meval `(($load) ,(cadr form)))
    (setq *package* (pop *namespace-stack*)))
  t)

(defun require-1 (s)
  (and
    (symbolp s)
    (or
      (find-package (symbol-name s))
      (mfuncall '$load_namespace s))))

(defmspec $require (e)
  `((mlist) ,@(mapcar #'require-1 (cdr e))))

(defun import-maxima-namespace (namespace-package)
  (when namespace-package
    (let (y)
      (do-external-symbols (s namespace-package)
        (shadowing-import s)
        (push s y))
      ($sort (cons '(mlist) y)))))

(defmspec $import (expr)
  `((mlist)
    ,@(mapcar
        #'(lambda (x) (import-maxima-namespace (meval x)))
        (cdr expr))))

;   in_namespace(<s>)
;       -- makes the namespace named by symbol <s> the current namespace
;
;   If there does not already exist a namespace <s>, create a new namespace
;   with that name.
;
;   in_namespace() (no argument) returns the current namespace.

(defmspec $in_namespace (l)
  (let ((y (cadr l)))
    (cond
      ((null y) *package*)
      ((symbolp y)
       (when (not (boundp y))
         (let ((yy (symbol-name y)) (previous-namespace *package*))
           (eval (list 'defpackage yy '(:use :maxima :common-lisp)))
           (setq $namespaces (append $namespaces (list y)))
           ; SEEMS LIKE THERE'S A DANGER OF NAME COLLISION HERE
           (intern yy previous-namespace)
           (set y (find-package yy))))
       (setq *package* (symbol-value y)))
      (t
        (if (eq (mop y) '$\|)
          (progn
            (meval `(($in_namespace) ,(cadr y)))
            (meval `(($in_namespace) ,(meval y))))
          (merror "in_namespace: argument ~S is not a | expression" y))))))

(defmspec $\| (l)
  (let ((x (cadr l)) (y (caddr l)))
    (if (symbolp x)
      (progn
        (if (and (boundp x) (not (packagep (symbol-value x))))
          (merror "|: ~S is a ~S, not a namespace" x (type-of (symbol-value x))))
        (if (boundp x)
          (let ((xx (symbol-value x)) (yy (symbol-name y)))
            (cond ((null (find-symbol yy xx)) (shadow yy xx)))
            (find-symbol yy xx))
          `(($\|) ,x ,y)))
      `(($\|) ,(meval x) ,y))))

($infix '$\| 203 204)
(putprop '$\| '(#\|) 'dissym)

(defun parse+meval-infix (op stuff)
  (let ((r (parse-infix op stuff)))
    `($any . ,(meval (cdr r)))))
(putprop '$\| #'parse+meval-infix 'led)

(setf $props (delete "|" $props :count 1 :test #'equal))

;; Following bits about exporting Maxima symbols is something of a mess.
;; The :maxima package contains lots and lots of symbols, many of which
;; are relevant only to some specific block of code; those shouldn't
;; be exported. I've made an attempt to determine which symbols should be
;; exported: all dollarified functions, any dollarified symbols which
;; were declared by DEMFVAR, and any symbols which begin with the letter #\m.
;; I wouldn't be surprised if a better policy could be invented.

;; Bottom line is, I'm going to burn in Hell for this hackery ...

(defun export-maxima-keywords ()
  (do-symbols (s :maxima)
    (when (or (get s 'nud) (get s 'led) (get s 'mheader))
      (export s))))

(defun export-maxima-dollarified-functions ()
  (do-symbols (s :maxima)
    (when (equal (car (exploden s)) #\$)
      (if (or (fboundp s) (mfboundp s))
        (export s)))))

(defun export-maxima-dollarified-variables ()
  (do-symbols (s :maxima)
    (when (equal (car (exploden s)) #\$)
      ;; DEFMVAR makes a (key, value) pair in the hash table *VARIABLE-INITIAL-VALUES*.
      (multiple-value-bind (value present-p) (gethash s *variable-initial-values*)
        (declare (ignore value))
        (if (or present-p ($constantp s))
          (export s))))))

(defun export-maxima-mfoo ()
  (do-symbols (s :maxima)
    (when
      (and
        ;; Symbol name begins with #\m -- that covers MPLUS, MTIMES, and a lot of other stuff ...
        (equal (car (exploden s)) #\m)
        ;; ... but exclude anything also present in :common-lisp, to avoid name collision.
        (not (find-symbol (symbol-name s) :common-lisp)))
      (export s))))

(defun export-maxima-all ()
  (do-symbols (s :maxima)
    (export s)))

(export-maxima-keywords)
(export-maxima-dollarified-functions)
(export-maxima-dollarified-variables)
(export-maxima-mfoo)
; (export-maxima-all)

;; DEFMVAR isn't really appropriate for these ...
;; Maybe figure out how to cover these in general export above.

(export '$namespaces)
(export '$maxima)
(export '$%%)
(export '$__)
(mapcar #'export (cdr $infolists))
(export '$infolists)
(export '$true)
(export '$false)
(export '$in)
(export '$equal)
(export '$notequal)
(export '$done)
(export '$unknown)
(export '$block)

; ------------------ begin modified existing Maxima functions ------------------

;; Intern symbols in the current package.
;; I think the right policy is this:
;; if the current package :uses :maxima, then intern in current package;
;; otherwise intern in :maxima.
;; I don't know how to detect :uses :maxima, maybe that is easy.

(defun intern-invert-case (string)
  (intern (maybe-invert-string-case string)))

(defun stripdollar-string (x)
  (if (and (stringp x) (equal (car (exploden x)) #\$))
    (subseq x 1)
    x))

(defmfun dimension-atom (form result)
  (cond ((and (symbolp form) (get form atom-context))
	 (funcall (get form atom-context) form result))
	((stringp form) (dimension-string (makestring form) result))
	((ml-typep form 'array)
	 (dimension-array-object form result))
    ((and (symbolp form) (symbol-package form) (not (equal (symbol-package form) *package*)))   ; NEW
     ;; !! THE FOLLOWING DOES NOT DISPLAY NESTED PACKAGES CORRECTLY (ONLY THE INNERMOST IS DISPLAYED)
     (dimension-infix                                                                           ; NEW
       `(($\|)                                                                                  ; NEW
         ,#-gcl (make-symbol (package-name (symbol-package form)))                              ; NEW
          #+gcl (make-symbol (let ((x (package-name (symbol-package form)))) (if (stringp x) x "(none)"))) ; NEW
         ,(make-symbol (symbol-name form)))                                                     ; NEW
       result))                                                                                 ; NEW
	(t (dimension-string (makestring form) result))))

(defmfun makestring (atom)
  (let (dummy)
    (cond ((numberp atom) (exploden atom))
      ((stringp atom)
       (setq dummy (coerce atom 'list))
       (if $stringdisp
         (cons #\" (nconc dummy (list #\")))
         dummy))
	  ((not (symbolp atom)) (exploden atom))
	  ((and (setq dummy (get atom 'reversealias))
		(not (and (member atom $aliases :test #'eq) (get atom 'noun))))
	   (exploden (stripdollar dummy)))
      ((equal (symbol-name atom) "") '())                                           ; NEW
	  ((not (eq (getop atom) atom))
       (makestring (getop atom)))
	  (t (setq dummy (exploden atom))
	     (cond
           ((null dummy) nil)
           ((char= #\$ (car dummy)) (cdr dummy))
		   ((char= #\% (car dummy)) (cdr dummy))
		   ($lispdisp (cons #\? dummy))
		   (t dummy))))))

(defmfun remvalue (x fn)
  (declare (special dcount))                                                ; NEW
  (cond ((not (symbolp x)) (improper-arg-err x fn))
	((boundp x)
	 (let (y)
	   (cond ((or (setq y (member x (cdr $values) :test #'equal))
		      (member x (cdr $labels) :test #'equal))
		  (cond (y (setf $values (delete x $values :count 1 :test #'eq)))
			(t (setf $labels (delete x $labels :count 1 :test #'eq))
			   (remprop x 'time) (remprop x 'nodisp)
			   (if (not (zerop dcount))
			       (setq dcount (1- dcount)))))
		  (makunbound x)
		  (when (member x *builtin-symbols-with-values* :test #'equal)
		    (setf (symbol-value x)
			  (gethash x *builtin-symbol-values*)))
		  t)
		 ((get x 'special)
		  (makunbound x)
		  (when (member x *builtin-symbols-with-values* :test #'equal)
		    (setf (symbol-value x)
			  (gethash x *builtin-symbol-values*)))
		    t)
		 (transp (setf (symbol-value x) x) t)
		 ((eq x '$default_let_rule_package) t)
         ((member x (cdr $namespaces) :test #'eq)                           ; NEW
          (setf $namespaces (delete x $namespaces :count 1 :test #'eq))     ; NEW
          (delete-package x)                                                ; NEW
          (makunbound x))                                                   ; NEW
		 (t
		  (mtell "Warning: Illegal `remvalue' attempt:~%~M" x) nil))))))

(defmfun kill1 (x)
  (declare (special allbutl dcount greatorder lessorder))                   ; NEW
  (if (and (stringp x) (not (getopr0 x))) (return-from kill1 nil))
  (funcall
   #'(lambda (z)
       (cond ((and allbutl (member x allbutl :test #'equal)))
	     ((eq (setq x (getopr x)) '$labels)
	      (dolist (u (cdr $labels))
		(cond ((and allbutl (member u allbutl :test #'equal))
		       (setq z (nconc z (ncons u))))
		      (t (makunbound u) (remprop u 'time)
			 (remprop u 'nodisp))))
	      (setq $labels (cons '(mlist simp) z) $linenum 0 dcount 0))
	     ((member x '($values $arrays $aliases $rules $props
			$let_rule_packages) :test #'equal)
	      (mapc #'kill1 (cdr (symbol-value x))))
	     ((member x '($functions $macros $gradefs $dependencies $structures) :test #'equal)
	      (mapc #'(lambda (y) (kill1 (caar y))) (cdr (symbol-value x))))
	     ((eq x '$myoptions))
	     ((eq x '$tellrats) (setq tellratlist nil))
	     ((eq x '$ratweights) (setq *ratweights nil
					$ratweights '((mlist simp))))
	     ((eq x '$features)
	      (cond ((not (equal (cdr $features) featurel))
		     (setq $features (cons '(mlist simp) (copy-list featurel))))))
         ((eq x '$namespaces)                            ; NEW
          (mapc #'kill1 (cdr (symbol-value x))))         ; NEW
	     ((or (eq x t) (eq x '$all))
	      (mapc #'kill1 (cdr $infolists))
	      (setq $ratvars '((mlist simp)) varlist nil genvar nil
		    checkfactors nil greatorder nil lessorder nil $gensumnum 0
		    $weightlevels '((mlist)) *ratweights nil $ratweights
		    '((mlist simp))
		    tellratlist nil $dontfactor '((mlist)) $setcheck nil)
	      (killallcontexts))
	     ((setq z (assoc x '(($inlabels . $inchar) ($outlabels . $outchar) ($linelabels . $linechar)) :test #'eq))
	      (mapc #'(lambda (y) (remvalue y '$kill))
		    (getlabels* (eval (cdr z)) nil)))
	     ((and (eq (ml-typep x) 'fixnum) (not (< x 0))) (remlabels x))
	     ((atom x) (kill1-atom x))
	     ((and (eq (caar x) 'mlist) (eq (ml-typep (cadr x)) 'fixnum)
		   (or (and (null (cddr x))
			    (setq x (append x (ncons (cadr x)))))
		       (and (eq (ml-typep (caddr x)) 'fixnum)
			    (not (> (cadr x) (caddr x))))))
	      (let (($linenum (caddr x))) (remlabels (- (caddr x) (cadr x)))))
	     ((setq z (mgetl (caar x) '(hashar array))) (remarrelem z x))
         ((eq (caar x) '$@) (mrecord-kill x))
	     ((and (eq (caar x) '$allbut)
		   (not (dolist (u (cdr x))
			  (if (not (symbolp u)) (return t)))))
	      (let ((allbutl (cdr x))) (declare (special allbutl)) (kill1 t)))              ; MODIFIED
	     (t (improper-arg-err x '$kill))))
   nil))

(defun msize-atom (x l r)
  (prog (y)
     (cond ((numberp x) (setq y (exploden x)))
       ((and                                                                            ; NEW
          (symbolp x)                                                                   ; NEW
          (symbol-package x)                                                            ; NEW
          (not (equal (symbol-package x) *package*)))                                   ; NEW
        (return                                                                         ; NEW
          ;; !! THE FOLLOWING DOES NOT DISPLAY NESTED PACKAGES CORRECTLY (ONLY THE INNERMOST IS DISPLAYED)
          (msize                                                                        ; NEW
            `(($\|)                                                                     ; NEW
              ,#-gcl (make-symbol (package-name (symbol-package x)))                    ; NEW
               #+gcl (make-symbol (let ((x (package-name (symbol-package x)))) (if (stringp x) x "(none)"))) ; NEW
              ,(make-symbol (symbol-name x)))                                           ; NEW
            l r lop rop)))                                                              ; NEW
	   ((and (setq y (safe-get x 'reversealias))
		 (not (and (member x $aliases :test #'eq) (get x 'noun))))
	    (setq y (exploden (stripdollar y))))
       ((null (setq y (exploden x))))
       ((safe-get x 'noun) (return (msize-atom (get x 'noun) l r)))
	   ((char= #\$ (car y)) (setq y (slash (cdr y))))
	   ((stringp x)
        (setq y (coerce x 'list))
	    (do ((l y (cdr l))) ((null l))
	      (cond ((or (member (car l) '(#\" #\\ #\; #\$) :test #'equal)
			 (and (char< (car l) #\space)
			      (not (char= (car l) #\return))))
		     (rplacd l (cons (car l) (cdr l)))
		     (rplaca l #\\) (setq l (cdr l)))))
	    (setq y (cons #\" (nconc y (list #\")))))
	   (t (setq y (cons #\? (slash y)))))
     (return (msz y l r))))

;; 2-D PRETTY PRINTER FUNCTIONS FOR OPERATORS
;; I SUPPOSE CORRESPONDING FUNCTIONS ARE NEEDED FOR GRIND

(defun get-dissym (a)                                          ; NEW
  (let ((b (get a 'dissym)) (p (symbol-package a)))            ; NEW
    (if (or (eq p *package*) (eq a '$\|))                      ; NEW
      b                                                        ; NEW
      (let ((c (exploden (stripdollar (package-name p)))))     ; NEW
        (if (consp (car b))                                    ; NEW
          (append (list (get-dissym-1 (car b) c)) (get-dissym-1 (rest b) c)) ; NEW
          (get-dissym-1 b c))))))                              ; NEW

(defun get-dissym-1 (b c)                                      ; NEW
  (if (eq (car b) #\space)                                     ; NEW
    (cons #\space (append c (cons #\| (rest b))))              ; NEW
    (append c (cons #\| b))))                                  ; NEW

(defmfun dimension-prefix (form result)
  (prog (dissym (symlength 0))
     (setq dissym (get-dissym (caar form))                     ; NEW
	   symlength (length dissym))
     (setq result (dimension (cadr form) (revappend dissym result) (caar form) rop symlength right)
	   width (+ symlength width))
     (return result)))

(defmfun dimension-infix (form result)
  (unless (= (length (cdr form)) 2)
    (return-from dimension-infix (dimension-function form result)))
  (prog (dissym (symlength 0) (w 0) (h 0) (d 0))
     (setq dissym (get-dissym (caar form))                     ; NEW
	   symlength (length dissym)
	   result (dimension (cadr form) result lop (caar form) 0 symlength)
	   w width
	   h height
	   d depth)
     (setq result (revappend dissym result))
     (checkbreak result (+ symlength w))
     (setq result (dimension (caddr form) result (caar form) rop (+ symlength w) right)
	   width (+ w symlength width)
	   height (max h height)
	   depth (max d depth))
     (return result)))

(defmfun dimension-nary (form result)
  ;; If only 0 or 1 arguments, then print "*"() or "*"(A)
  (cond ((null (cddr form))
	 (dimension-function form result))
	(t
	 (prog (dissym (symlength 0) (w 0) (h 0) (d 0) helper)
	    (setq dissym (get-dissym (caar form))              ; NEW
		  symlength (length dissym)

          ;; Look for a helper function. Fall back on default if none found.
          helper (or (safe-get (caar form) 'dimension-nary-helper) 'dimnary)

		  result (funcall helper (cadr form) result lop (caar form) (caar form) 0)
		  w width
		  h height
		  d depth
          )
	    (do ((l (cddr form) (cdr l)))
		(nil)
	      (checkbreak result w)
	      (setq result (revappend dissym result) w (+ symlength w))
	      (cond ((null (cdr l))
		     (setq result (funcall helper (car l) result (caar form) (caar form) rop w)
			   width (+ w width)
			   height (max h height)
			   depth (max d depth))
		     (return t))
		    (t
		     (setq result (funcall helper (car l) result (caar form) (caar form) (caar form) w)
			   w (+ w width)
			   h (max h height)
			   d (max d depth)))))
	    (return result)))))

(defmfun dimension-postfix (form result)
  (prog (dissym (symlength 0))
     (setq dissym (get-dissym (caar form))                     ; NEW
	   symlength (length dissym))
     (setq result (dimension (cadr form) result lop (caar form) 0 (+ symlength right))
	   width (+ symlength width))
     (return (revappend dissym result))))

(defmfun dimension-nofix (form result)
  (setq form (get-dissym (caar form))                          ; NEW
	width (length form))
  (revappend form result))

(defun dimension-match (form result)
  (prog (dissym (symlength 0))
     (setq dissym (get-dissym (caar form))                     ; NEW
	   symlength (length (car dissym)))
     (cond ((null (cdr form))
	    (setq width (+ symlength (length (cdr dissym)))
		  height 1
		  depth 0)
	    (return (revappend (cdr dissym) (revappend (car dissym) result))))
	   (t (setq result (let ((lop 'mparen)
				 (rop 'mparen)
				 (break (if break (+ symlength break)))
				 (right (+ symlength right)))
			     (dimension-list form (revappend (car dissym) result))))
	      (setq width (+ (length (cdr dissym)) symlength width))
	      (return (revappend (cdr dissym) result))))))


