;;;;
;;;;                                   ~*~  PRINTF  ~*~
;;;;
;;;;  formatted printing to character stream
;;;;
;;;;  Version       : 3.0 (march 2008)
;;;;  Copyright     : 2005-2008 Volker van Nek
;;;;  Licence       : GPL2
;;;;
;;;;  Test file     : rtestprintf.mac (lots of examples)
;;;;  Documentation : stringproc.texi (will be updated soon)
;;;;


;;    `$printf' is based on the Lisp function `format'.
;;    Version 3.0 of `$printf' now allows to format bigfloat numbers.
;;
;;    Before passing to `format' ctrls and args are both visited and modified:
;;    functions `prepare-args' and `prepare-ctrls':
;;    `prepare-ctrls' replaces the here introduced bigfloat directive ~w,d,e,x,o,p@h by ~@a 
;;    and `prepare-args' replaces the corresponding bigfloat argument by a string. 
;;    As a consequence of this no arg can be used twice. The *-directive (goto) seems lost.
;;    In addition: Args which are passed to d,f,e,g or h-directives are converted to that type.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;; new implementation of formerly src/plot.lisp/$sprint in stringproc/printf.lisp 
;; as a simple wrapper for printf(true,"~@{~a ~}",x,y,z,...)
;; keeping the original return value: the first argument resp. false
;;
(defun $sprint (&rest args)
  (loop for v in args do
    ($printf t "~a " v))
  (car args))


;;
;;
(defun $printf (stream ctrls &rest args)
  (cond ((and (not (member stream '(t nil))) (not (streamp stream)))
           (merror "printf: first argument must be `true', `false' or a stream."))
        ((not (stringp ctrls))
           (merror "printf: second argument must be a string.")))
  (let (body)
    (setq args (prepare-args ctrls args nil))
    (setq body (mapcar #'(lambda (x) (if (listp x) `(quote ,x) x)) args))
    (setq ctrls (prepare-ctrls ctrls))
    (eval `(format ,stream ,ctrls ,@body)) ))
;;
;;
;;
(defun prepare-args (ctrls todo done &optional (loop nil))
  (let ((start 0) pos1 pos2 pos1a
         params spec subctrls 
        (skip 0) (loops nil) (index 0))
    (do ((arg (car todo)))
        ((null todo)
           (progn
             (setq pos1 (search "~" ctrls :start2 start))
             (if pos1
               (progn
                 (setq pos2 (spec-position ctrls pos1))
                 (setq spec (subseq ctrls (1- pos2) pos2)) ))
             (if (and (zerop skip) (or (not pos1) (search spec "^{}[]<>%;&~tpTP
")));; newline possible spec
               (reverse done)
               (merror "printf: arguments exhausted."))))
      (prog ()
       tag1
;; recognize the directive:
        (setq pos1 (search "~" ctrls :start2 start))
        (if (not pos1)
          (if loop 
            (progn
              (setq start 0)
              (setq pos1 (search "~" ctrls :start2 start)))
            (progn
              (setq done (cons arg done))
              (go tag3) )))
        (setq pos2 (setq pos1a (spec-position ctrls pos1)))
        (setq spec (subseq ctrls (1- pos2) pos2))
        (if (search spec "}]>;%&t~") (progn (setq start pos2) (go tag1)))
        (setq params (subseq ctrls (1+ pos1) (1- pos2)))
;;
;; pre-test for ~nr, ~vr, ~#r :
;; check if radix is legal (Maxima 5.14 gcl segfaults when radix=1)
        (if (and (string-equal spec "r") (string/= "" params))
          (let ((c (subseq params 0 1)) (n "") (len (length params)) radix)
            (if (or ($digitcharp c) (search c "v#V"))
              (progn
                (do ((p 1 (1+ p)))
                    ((or (= p len)(search c ",@:v#V")))
                  (progn
                    (setq n (concatenate 'string n c))
                    (setq c (subseq params p (1+ p))) ))
                (setq radix
                  (cond
                    ((string= c ",") 10)
                    ((string-equal c "v") arg)
                    ((string= c "#") (length todo))
                    ((or (string= c "@") (string= c ":")) (parse-integer n))
                    (t (parse-integer (concatenate 'string n c))) ))
                (if (or (< radix 2) (> radix 36))
                  (merror "printf: illegal radix in r-directive: ~m" radix)) ))))
;;
;; handle some special directives:
        (cond
;; ~v,v,v<spec>, spec=ABDEFGORSTX<~&%$   (# needs no special care; ~v[ not supported, see below)
          ((search spec "abdefgorstx<~&%$" :test #'string-equal)
             (if (plusp (setq skip (count-v params)))
               (progn
                 (do () ((zerop skip))
                   (progn
                     (setq done (cons (if (stringp arg) (character arg) arg) done))
                     (setq arg (car (setq todo (cdr todo))))
                     (setq index (1+ index))
                     (setq skip (1- skip)) ))
                 (setq done (cons (prepare-arg params spec arg) done))
                 (go tag2) )))
;; ~v,#,vH
          ((string-equal spec "h")
            (if (check-v# params)
              (let ((prms (split-at-comma params))
                     prm (new-params ""))
                (dolist (p prms)
                  (progn
                    (setq prm
                      (cond
                        ((string-equal "v" p)
                           (prog1
                             ($sconcat arg)
                             (setq arg (car (setq todo (cdr todo))))
                             (setq index (1+ index))))
                        ((string= "#" p)
                           ($sconcat (length todo)))
                        (t p)))
                    (setq new-params (concatenate 'string new-params prm ",")) ))
                (setq done (cons (prepare-arg new-params spec arg) done))
                (go tag2) )))
;; ~@[, ~#[, ~n[
          ((string= "[" spec)
            (cond
              ((string= "" params)) ;; don't check another condition
              ((or (and (string= "@" params) arg) ;; if arg is not nil, arg is not consumed
                   (string= "#" params)) 
                 (setq start pos2)
                 (go tag1))
              ((or (string= "v" params)
                   (every #'digit-char-p (coerce params 'list)))
                 (merror "printf: not supported directive ~~~m[" params)) )) ;; 0- vs. 1-based indexing
;; ~?
          ((string= "?" spec)
            (cond
              ((string= "" params)
                 (let ((ind-ctrls arg)) ;; arg is a string
                   (setq done (cons arg done))
                   (setq arg (car (setq todo (cdr todo))))
                   (setq index (1+ index))
                   (setq done
                     (cons (prepare-args ind-ctrls (cdr arg) nil nil) done) )
                   (go tag2) ))
              ((string= "@" params)
                 (setq ctrls 
                   (concatenate 'string (subseq ctrls 0 pos1) arg (subseq ctrls pos2)))
                 (setq done (cons arg done))
                 (go tag3))
              (t 
                 (merror "printf: illegal directive ~~~m?" params)) ))
;; ~^
          ((string= "^" spec)
            (if (search "@" params)
              (merror "printf: illegal directive ~~~m^" params))
              (progn 
                (if (plusp (setq skip (count-v params)))
                  (progn
                    (do () ((zerop skip))
                      (progn
                        (setq done (cons (if (stringp arg) (character arg) arg) done))
                        (setq arg (car (setq todo (cdr todo))))
                        (setq index (1+ index))
                        (setq skip (1- skip)) ))
                    (setq done (cons (prepare-arg params spec arg) done)) ))
                (setq start pos2)
                (go tag1) ))
;; ~:P and ~:@P
          ((and (string-equal "p" spec) (search ":" params)) ;; ':' backs up
             (setq start pos2)
             (go tag1))
        )
;; default part:
;;
;; loop ... 
        (if (string= "{" spec)
          (progn
;; ~n{ and ~v{ etc. , set number of loops
            (if (and (string/= "" params) 
                     (string/= ":" params) (string/= "@" params) (string/= ":@" params))
              (let ((c (subseq params 0 1)) (n "") (len (length params)))
                (do ((p 1 (1+ p)))
                    ((or (= p len)(search c "@:v#V"))
                       (setq params
                         (if (or (string= c "@") (string= c ":"))
                           (subseq params (1- p))
                           (subseq params p))))
                  (progn
                    (setq n (concatenate 'string n c))
                    (setq c (subseq params p (1+ p))) ))
                (and (not loops) 
                  (cond
                    ((string-equal c "v") 
                       (setq loops arg)
                       (setq done (cons arg done))
                       (setq arg (car (setq todo (cdr todo))))
                       (setq index (1+ index)) )
                    ((string= c "#") 
                       (setq loops (length todo)) )
                    ((or (string= c "@") (string= c ":")) 
                       (setq loops (parse-integer n)) )
                    (t 
                       (setq loops (parse-integer (concatenate 'string n c))) ))) ))
;; ~{ and ~:{ and ~@{ and  ~:@{ 
            (cond 
              ((string= "" params)
                 (setq pos2 (cadr (iter-positions ctrls pos1)))
                 (setq subctrls (subseq ctrls pos1a (- pos2 2)))
                 (setq done (cons (prepare-args subctrls (cdr arg) nil t) done))
                 (and loops (setq loops nil)) )
              ((string= ":" params)
                 (setq pos2 (cadr (iter-positions ctrls pos1)))
                 (setq subctrls (concatenate 'string "~{" (subseq ctrls pos1a pos2)))
                 (setq done (cons (prepare-args subctrls (cdr arg) nil t) done))
                 (and loops (setq loops nil)) )
              ((string= "@" params)
                 (setq pos2 (cadr (iter-positions ctrls pos1)))
                 (setq subctrls (concatenate 'string "~{" (subseq ctrls pos1a pos2)))
                 (setq done 
                   (append 
                     (reverse 
                       (car 
                         (prepare-args 
                           subctrls 
                           (list (cons '(mlist) (if loops (butlast todo (- (length todo) loops)) todo) ))
                           nil nil))) 
                     done))
                 (setq todo 
                   (if loops (nthcdr loops todo) nil))
                 (and loops (setq index (+ index loops)))
                 (setq arg (car todo))
                 (and loops (setq loops nil))
                 (setq start pos2)
                 (go tag4))
              ((string= ":@" params)
                 (setq pos2 (cadr (iter-positions ctrls pos1)))
                 (setq subctrls 
                   (concatenate 'string "~" (if loops ($sconcat loops) "") ":{" (subseq ctrls pos1a pos2)))
                 (setq done 
                   (append 
                     (reverse 
                       (car 
                         (prepare-args 
                           subctrls 
                           (list (cons '(mlist) (if loops (butlast todo (- (length todo) loops)) todo) ))
                           nil nil))) 
                     done))
                 (setq todo 
                   (if loops (nthcdr loops todo) nil))
                 (and loops (setq index (+ index loops)))
                 (setq arg (car todo))
                 (and loops (setq loops nil))
                 (setq start pos2)
                 (go tag4)) ))
;; ... or don't loop ...
          (setq done 
            (cons (prepare-arg params spec arg) done))) 
;; ... set the position in ctrls ...
       tag2
        (setq start pos2)
;; ... and take the next argument
       tag3 
        (setq arg (car (setq todo (cdr todo))))
        (setq index (1+ index))
       tag4 ))))
;;
;;
;;
(defun prepare-ctrls (ctrls)
  (let ((start 0) (pos1 nil) (pos2 0)
        (new-ctrls "")) 
;; ~w,d,e,x,o,p@H
    (setq pos1 (search "~" ctrls :start2 start))
    (do () 
        ((not pos1)
           (concatenate 'string new-ctrls (subseq ctrls pos2)))
      (progn
        (setq pos2 (spec-position ctrls pos1))
        (setq new-ctrls 
          (if (string-equal (subseq ctrls (1- pos2) pos2) "h")
            (concatenate 'string new-ctrls (subseq ctrls start pos1) "~@a")
            (concatenate 'string new-ctrls (subseq ctrls start pos2)) ))
        (setq start pos2) 
        (setq pos1 (search "~" ctrls :start2 start)) ))))
;;
;;
;;
(defun spec-position (ctrls pos1)
  (do ((p (1+ pos1) (1+ p))) (())
    (if (and (search (subseq ctrls p (1+ p)) "abcdefghoprstx%{}^&$[]?~<>;ABCDEFGHOPRSTX
") ;;  newline possible spec
             (not (string= "'" (subseq ctrls (1- p) p))))
      (return (1+ p)) )))
;;
;;
;;  helper for ~v,v,v<spec>
;;
(defun count-v (params)
  (if (string= "" params)
    0
    (do ((p 0 (1+ p)) (len (length params)) (n 0))
        ((= p len) n)
      (if (and (string-equal "v" (subseq params p (1+ p)))
               (or (zerop p) (not (string= "'" (subseq params (1- p) p)))))
        (setq n (1+ n)) ))))
;;
;;
;;  helper for ~v,#,vH
;;
(defun check-v# (params)
  (if (string= "" params)
    nil
    (do ((p 0 (1+ p)) (len (length params)))
        ((= p len) nil)
      (if (and (search (subseq params p (1+ p)) "vV#")
               (or (zerop p) (not (string= "'" (subseq params (1- p) p)))))
        (return t)))))
;;
;;
;;  find positions of matching braces
;;
(defun iter-positions (ctrls start)
  (let ( pos1 pos2 (end (+ start 2))
         spec (n 1))
    (do ()
        ((zerop n) (list start end))
      (progn
        (setq pos1 (search "~" ctrls :start2 end))
        (setq pos2 (spec-position ctrls pos1))
        (setq spec (subseq ctrls (1- pos2) pos2))
        (if (string-equal spec "{") (setq n (1+ n)))
        (if (string-equal spec "}") (setq n (1- n)))
        (setq end pos2) ))))
;;
;;
(defun split-at-comma (params)
    (do ((p 0 (1+ p)) (len (length params)) (prms nil) (prm ""))
        ((= p len) (reverse (cons prm prms)))
      (cond 
        ((and (search (subseq params p (1+ p)) ",@")
              (or (zerop p) (not (string= "'" (subseq params (1- p) p)))))
           (setq prms (cons prm prms))
           (setq prm ""))
        ((and (string= (subseq params p (1+ p)) ",")
              (not (zerop p))
              (string= "'" (subseq params (1- p) p)))
           (setq prms (cons "," prms))
           (setq prm ""))
        ((string= (subseq params p (1+ p)) "'")
            nil)
        (t
           (setq prm (concatenate 'string prm (subseq params p (1+ p))))) )))
;;
;;
(defun prepare-arg (params spec arg)
  (cond
;; ~w,d,e,x,o,p@H
        ((string-equal "h" spec)
           (if (not (bigfloatp arg)) 
             (cond
               (($numberp arg) ;; Maxima rational, float
                 (setq arg ($bfloat arg)))
               ((and ($constantp arg) ;; %pi, sqrt(2), ...
                     ($freeof '$%i arg) (not (member arg '(t nil))) (not ($listp arg)))
                 (setq arg ($bfloat arg)))
               (t
                 (merror "printf: argument can't be supplied to h-directive: ~m" arg))))
           (let ((prms (split-at-comma params))
                  w d e x o p at smarap)
             (multiple-value-setq (w d e x o p) (apply #'values prms))
             (setq smarap (reverse params))
             (if (and (string/= "" smarap) 
                      (string= (subseq smarap 0 1) "@") 
                      (or (= 1 (length smarap)) (string/= (subseq smarap 1 2) "'")))
               (setq at t))
             (setq arg 
               (let ((w (if (and w (string/= "" w)) (parse-integer w)))
                     (d (if (and d (string/= "" d)) (parse-integer d)))
                     (e (if (and e (string/= "" e)) (parse-integer e))) 
                     (x (if (and x (string/= "" x)) (parse-integer x))) )
                 (bprintf arg w d e x o p at))) ))
;; ~E, ~F, ~G
        ((search spec "efg" :test #'string-equal)
           (if (not (floatp arg))
             (cond
               (($numberp arg) ;; Maxima rational, bigfloat
                 (setq arg ($float arg)))
               ((and ($constantp arg) ;; %pi, sqrt(2), ...
                     ($freeof '$%i arg) (not (member arg '(t nil))) (not ($listp arg)))
                 (setq arg ($float arg)))
               (t
                 (merror "printf: argument can't be supplied to ~m-directive: ~m" spec arg)))))
;; ~D
        ((string-equal "d" spec)
           (if (not (integerp arg))
             (cond
               (($numberp arg) ;; Maxima rational, (big)float
                 (setq arg ($truncate arg)))
               ((and ($constantp arg) ;; %pi, sqrt(2), ...
                     ($freeof '$%i arg) (not (member arg '(t nil))) (not ($listp arg)))
                 (setq arg ($truncate arg)))
               (t
                 (merror "printf: argument can't be supplied to d-directive: ~m" arg)))))
;; ~A, ~S
        ((search spec "as" :test #'string-equal)
           (setq arg ($sconcat arg)))
;; ~C
        ((string-equal "c" spec)
           (setq arg (character arg))) ;; conversion to Lisp char
;; ~[
        ((string= "[" spec) 
           (if (or (string= "@" params) (string= ":" params))
             arg
             (if (integerp arg) (setq arg (1- arg))) ))) ;; 1-based indexing!
  arg) 
;;
;;
;;
;; bf: bigfloat
;; wd: nil or width
;; dd: nil or decimal digits behind floating point
;; ed: nil or minimal exponent digits
;; xp: nil or prefered exponent
;; ov: nil or overflow character
;; pc: nil or padding character
;; at: nil or true; sets "+" if true
;;
(defun bprintf (bf wd dd ed xp ov pc at);; ~w,d,e,x,o,p@H
;;
  (and xp (not (zerop xp))
    (setq bf (bcons (if (minusp xp) 
                      (fptimes* (cdr bf) (intofp (expt 10 (- xp))))
                      (fpquotient (cdr bf) (intofp (expt 10 xp))) ))))
;;
  (and dd
    (let ((m (intofp (expt 10 dd))))
      (setq bf (fptimes* (cdr bf) m))
      (setq bf (meval `((%round) ,(bcons bf))))
      (setq bf (bcons (fpquotient (intofp bf) m))) ))  
;;
  (let* ((s (string-left-trim "-" ($sconcat bf)))
         (sgn (signum (cadr bf)))
         (part1 (subseq s 0 1))
         (pos (position #\b s))
         (part2 (string-right-trim "0" (subseq s 2 pos)))
         (len (length part2))
         (pow (parse-integer (subseq s (1+ pos) nil))))
    (cond ((and (> pow 0) (> len pow))
             (setq s (concatenate 'string part1 (subseq part2 0 pow) "." (subseq part2 pow nil)))
             (and dd (> dd (- len pow))
               (setq s (concatenate 'string s (make-string (+ dd pow (- len)) :initial-element #\0)))))
          ((> pow 0)
             (setq s (concatenate 'string part1 part2 (make-string (- pow len) :initial-element #\0) ".0"))
             (and dd (> dd 0)
               (setq s (concatenate 'string s (make-string (1- dd) :initial-element #\0)))))
          ((zerop pow)
             (setq s (concatenate 'string part1 (if (zerop len) ".0" ".") part2))
             (if (zerop len) (setq len (1+ len)))
             (and dd (> dd len)
               (setq s (concatenate 'string s (make-string (- dd len) :initial-element #\0)))))
          ((< pow 0)
             (setq s (concatenate 'string "0." (make-string (- 0 pow 1) :initial-element #\0) part1 part2))
             (and dd (> dd (- len pow))
               (setq s (concatenate 'string s (make-string (+ dd pow (- len)) :initial-element #\0))))))
;;
    (and dd (zerop dd)
      (setq s (string-right-trim "0" s)))
    (if (minusp sgn) 
      (setq s (concatenate 'string "-" s))
      (if at (setq s (concatenate 'string "+" s))))
;;
    (if (or ed xp)
      (let (xps xpl)
        (if (not xp) (setq xp 0))
        (setq xps (meval `(($string) ,xp)))
        (if (minusp xp)
          (setq xps (subseq xps 1)))
        (setq xpl (length xps))
        (if (and ed (plusp (- ed xpl 1)))
          (setq xps (concatenate 'string (make-string (- ed xpl 1) :initial-element #\0) xps)))
        (setq s (concatenate 'string s "b" (if (minusp xp) "-" "+") xps))))
;;
    (setq len (length s))
    (and wd ov (> len wd)
      (setq s (make-string wd :initial-element (character ov))))
    (and wd (> wd len)
      (setq s (concatenate 'string (make-string (- wd len) :initial-element (if pc (character pc) #\ )) s)))
;;
    s ))
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
