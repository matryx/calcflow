(in-package :maxima)

(declaim (optimize (safety 2) (space 3)))

(eval-when
    #+gcl (compile eval)
    #-gcl (:compile-toplevel :execute)

    (defmacro f (op &rest args)
      `(the fixnum (,op ,@ (mapcar #'(lambda (x) `(the fixnum ,x)) args))))

    (defmacro fb (op &rest args)
      `(,op ,@ (mapcar #'(lambda (x) `(the fixnum ,x)) args))))

;; This function is not documented and not used in Maxima core or share.
(defun $bt()
  (loop for v in *baktrcl*
	 do 
	 (and (consp v)
	      (consp (cadar v))
	      (eq (caadar v) 'src))
	 ($print (format nil "~a:~a:" (nth 1 (cadar v))
			 (nth 0 (cadar v))) v)))

;; *mlambda-call-stack*
;; #(NIL ($X) (1) $FF ($BIL $X) ($Y) (36) $JOE ($Y $BIL $X) ($JJX) (36)
;; to get to current values in ff need to unbind bindlist downto ($BIL $X)
;; to get to current values in joe need to unbind bindlist downto ($Y $BIL $X)

(defvar *current-frame* 0)

(defvar $mdebug_print_length 100 "Length of forms to print out in debugger")

(defmacro bak-top-form (x) x)

(defun frame-info (n)
  (declare (fixnum n))
  (let* ((ar *mlambda-call-stack*)
	 (m (length ar))
	 fname vals params backtr lineinfo bdlist)
    (declare (fixnum m))
    ;; just in case we do not have an even multiple
    (setq m (f - m (f mod m 5) (* n 5)))
    (if (<= m 0) (return-from frame-info nil))
    (setq fname (aref ar (f- m 1)))
    (setq vals (aref ar (f- m 2)))
    (setq params (aref ar (f- m 3)))
    (setq backtr (aref ar (f- m 4)))
    (setq bdlist (if (< m (fill-pointer ar)) (aref ar m) bindlist))
					; (setq lineinfo (get-lineinfo backtr))
    (setq lineinfo (if ( < m (fill-pointer ar))
		       (get-lineinfo (bak-top-form (aref ar (f+ m 1))))
		       (get-lineinfo (bak-top-form *last-meval1-form*))))
    ;;    #+if-you-use-baktrcl 
    ;;	  (if ( < m (fill-pointer ar))
    ;;	      (get-lineinfo (bak-top-form (aref ar (f+ m 1))))
    ;;	    (or (get-lineinfo (bak-top-form *last-meval1-form*
    ;;					    ;baktrcl
    ;;					    ))
    ;;		;(get-lineinfo (bak-top-form (cdr baktrcl)))
    ;;		))
    (values fname vals params backtr lineinfo bdlist)))

(defun print-one-frame (n print-frame-number &aux val (st *debug-io*))
  (multiple-value-bind
	(fname vals params backtr lineinfo bdlist)
      (frame-info n)
    (cond (fname
	   (princ (if print-frame-number
		      ($sconcat "#" n ": " fname "(")
		      ($sconcat  fname "("))
		  st)
	   (loop for v on params for w in vals
		  do (setq val ($sconcat w))
		  (if (> (length val) 100)
		      (setq val ($sconcat (subseq val 0 100) "...")))
		  (format st "~(~a~)=~a~a" ($sconcat (car v)) val
			  (if (cdr v) "," "")))
	   (princ ")" st)
	   (and lineinfo
		(format st (intl:gettext "(~a line ~a)")
			(short-name (cadr lineinfo)) (car lineinfo)))
	   (terpri st)
	   (values fname vals params backtr lineinfo bdlist))
	  (t nil))))

;; these are in the system package in gcl...
#-gcl
(progn 'compile
       (defun break-call (key args prop &aux fun)
	 (setq fun (complete-prop key 'keyword prop))
	 (setq key fun)
	 (or fun (return-from break-call nil))
	 (setq fun (get fun prop))
	 (unless (symbolp fun)
	   (let ((gen (gensym)))
	     (setf (symbol-function gen) fun) (setf (get key prop) gen)
	     (setq fun gen)))
	 (cond (fun
		(setq args (cons fun args))
					; jfa temporary hack
		#+gcl(evalhook args nil nil *break-env*)
		#-gcl(eval args)
	        )
	       (t (format *debug-io* 
	                  (intl:gettext "~&~S is an undefined break command.~%")
			  key))))

       (defun complete-prop (sym package prop &optional return-list)
	 (cond ((and (symbolp sym)(get sym prop)(equal (symbol-package sym)
						       (find-package package)))
		(return-from complete-prop sym)))
	 (loop for vv being the symbols of package 
		when (and (get vv prop)
			  (eql #+gcl (string-match sym vv)
			       #-gcl (search (symbol-name sym)
					     (symbol-name vv)) 
			       0))
		collect vv into all
		finally
       
		(cond (return-list (return-from complete-prop all))
		      ((> (length all) 1)
               ;; NOTE TO TRANSLATORS: MEANING OF FOLLOWING IS UNKNOWN
		       (format t
		         (intl:gettext 
                           "~&Not unique with property ~(~a: ~{~s~^, ~}~).")
			       prop all))
		      ((null all)
		       (format t 
		         (intl:gettext "~& No such break command: ~a") sym))
		      (t (return-from complete-prop
			   (car all)))))))

(defun $backtrace (&optional (n 30))
  (let ($display2d (st *debug-io*))
    (loop for i below n
	   for j from *current-frame*
	   while (print-one-frame j t))))

;; the following are in the maxima package....
;; they are DIFFERENT from ones in si package..

;; if this is NIL then nothing more is checked in eval

(defvar *break-points* nil)
(defvar *break-point-vector* (make-array 10 :fill-pointer 0 :adjustable t))

(defun init-break-points ()
  (setf (fill-pointer *break-point-vector*) 0)
  (setf *break-points* *break-point-vector*))

(defvar *break-step* nil)
(defvar *step-next* nil)

(defun step-into (&optional (n 1))
  ;;FORM is the next form about to be evaluated.
  n
  (or *break-points* (init-break-points))
  (setq *break-step* 'break-step-into)
  :resume)

(defun step-next (&optional (n 1))
  n
  (let ((fun (current-step-fun)))
    (setq *step-next* (cons n fun))
    (or *break-points* (init-break-points))
    (setq *break-step* 'break-step-next)
    :resume))

(defun maybe-break (form line-info fun env &aux pos)
  (declare (ignore env))
  (cond ((setq pos (position form line-info))
	 (setq *break-step* nil)
	 (or (> (length *break-points*) 0)
	     (setf *break-points* nil))
	 (break-dbm-loop (make-break-point fun line-info pos))
	 t)))

;; These following functions, when they are the value of *break-step*
;; are invoked by an inner hook in eval. They may choose to stop things.
(defvar *break-step* nil)
(defun break-step-into (form &optional env)
  (let ((fun (current-step-fun)))
    (let ((line-info (set-full-lineinfo fun)))
      (and line-info 
	   (maybe-break form line-info fun env)))))

(defun break-step-next (form &optional env)
  (let ((fun (current-step-fun)))
    (cond ((eql (cdr *step-next*) fun)
	   (let ((line-info (set-full-lineinfo fun)))
	     (maybe-break form line-info fun env))))))

(defvar *lineinfo-array-internal* nil)

;; the lineinfo for a function will be a vector of forms
;; such that each one is the first form on a line.
;; we will walk thru the tree taking the first occurrence
;; for each line.
(defun set-full-lineinfo (fname &aux te)
  (let ((body (get fname 'lineinfo)))
    (cond ((atom body) (return-from set-full-lineinfo body))
	  (t (cond ((null *lineinfo-array-internal*)
		    (setq *lineinfo-array-internal*
			  (make-array 20 :fill-pointer 0 :adjustable t)))
		   (t (setf (fill-pointer *lineinfo-array-internal*) 0)))
	     (cond ((setq te (get-lineinfo body))
		    (vector-push (car te) *lineinfo-array-internal*)
		    (walk-get-lineinfo body  *lineinfo-array-internal*)))
	     (cond ((> (fill-pointer *lineinfo-array-internal*) 0)
		    (setf (get fname 'lineinfo)
			  (copy-seq *lineinfo-array-internal*)))
		   (t (setf (get fname 'lineinfo) nil)))))))

(defun walk-get-lineinfo (form ar &aux (i 0) tem)
  (declare (type (vector t) ar) (fixnum i))
  (cond ((atom form) nil)
	((setq tem (get-lineinfo form))
	 (setq i (f - (line-info-line tem) (aref ar 0) -1))
	 (cond ((< i (fill-pointer ar))
		(or (aref ar i)
		    (setf (aref ar i) form)))
	       (t
		(unless (< i (array-total-size ar))
		  (setq ar (adjust-array ar (+ i 20) :fill-pointer 
		                         (fill-pointer ar))))
		(loop for j from (fill-pointer ar) below i
		   do (setf (aref ar j) nil))
		(setf (fill-pointer ar) (f + i 1))
		(setf (aref ar i) form)))
	 (loop for v in (cdr form)
	    do (or (atom v)
		   (walk-get-lineinfo v ar))))))

(defun first-form-line (form line &aux tem)
  (cond ((atom form) nil)
	((and (setq tem (get-lineinfo form)) (eql (car tem) line))
	 form)
	(t (loop for v in (cdr form)
		  when (setq tem (first-form-line v line))
		  do (return-from first-form-line tem)))))

(defvar *last-dbm-command* nil)

;; split string into a list of strings, split by any of a list of characters
;; in bag.  Returns a list.  They will have fill pointers..
(defun split-string (string  bag &optional (start 0) &aux all pos v l)
  (declare (fixnum start) (type string string))
  (loop for i from start below (length string)
	 do (setq pos (position (setq v (aref string i)) bag))
	 (setq start (+ start 1))
	 (cond ((null pos) (push v all))
	       (t (if all (loop-finish))))
	 finally
	 (if all
	     (return-from split-string
	       (cons
		(make-array (setq l (length all))
			    :fill-pointer l
			    :adjustable t
			    :initial-contents (nreverse all)
			    :element-type
			    ' #.(array-element-type "ab"))
		(split-string string bag start))))))

(declaim (special *mread-prompt*))

;; RLT: What is the repeat-if-newline option for?  A grep of the code
;; indicates that dbm-read is never called with more than 3 args.  Can
;; we just flush it?  Can probably get rid of the &aux stuff too.

(defvar *need-prompt* t)

(defun dbm-read (&optional (stream *standard-input*) (eof-error-p t)
		 (eof-value nil) repeat-if-newline  &aux tem  ch
		 (mprompt *mread-prompt*) (*mread-prompt* ""))
  (if (and *need-prompt* (> (length mprompt) 0))
    (progn
      (fresh-line *standard-output*)
      (princ mprompt *standard-output*)
      (force-output *standard-output*)
      (setf *prompt-on-read-hang* nil))
    (progn
      (setf *prompt-on-read-hang* t)
      (setf *read-hang-prompt* mprompt)))

  ;; Read a character to see what we should do.
  (tagbody
   top
     (setq ch (read-char stream eof-error-p eof-value))
     (cond ((or (eql ch #\newline) (eql ch #\return))
	    (if (and repeat-if-newline *last-dbm-command*)
		(return-from dbm-read *last-dbm-command*)) 
	    (go top))
	   ((eq ch eof-value)
	    (return-from dbm-read eof-value)))
     ;; Put that character back, so we can reread the line correctly.
     (unread-char ch stream))

  ;; Figure out what to do
  (cond ((eql #\: ch)
	 ;; This is a Maxima debugger command (I think)
	 (let* ((line (read-line stream eof-error-p eof-value))
		fun)
	   (multiple-value-bind
		 (keyword n)
	       (read-from-string line)
	     (setq fun (complete-prop keyword 'keyword 'break-command))
	     (and (consp fun) (setq fun (car fun)))
	     ;;(print (list 'line line))
	     (setq *last-dbm-command*
		   (cond ((null fun) '(:_none))
			 ((get fun 'maxima-read)
			  (cons keyword (mapcar 'macsyma-read-string
						(split-string line " " n))))
			 (t (setq tem
				  ($sconcat "(" (string-right-trim  ";" line)
                                            ")"))
			    ;;(print (list 'tem tem))
			    (read  (make-string-input-stream tem)
				   eof-error-p eof-value)))))))
	((eql #\? ch)
	 ;; Process "?" lines.  This is either a call to describe or a
	 ;; quick temporary escape to Lisp to call some Lisp function.
	 
	 ;; First, read and discard the #\? since we don't need it anymore.
	 (read-char stream)
	 (let ((next (peek-char nil stream nil)))
	   (cond ((member next '(#\space #\tab #\!))
		  ;; Got "? <stuff>" or "?! <stuff>".
	          ;; Invoke exact search on <stuff>.
		  (let* ((line (string-trim 
				'(#\space #\tab #\; #\$)
				(subseq 
				 (read-line stream eof-error-p eof-value) 1))))
		    `((displayinput) nil (($describe) ,line $exact))))
         ((equal next #\?)
          ;; Got "?? <stuff>". Invoke inexact search on <stuff>.
		  (let* ((line (string-trim 
				'(#\space #\tab #\; #\$)
				(subseq 
				 (read-line stream eof-error-p eof-value) 1))))
		    `((displayinput) nil (($describe) ,line $inexact))))
		 (t
		  ;; Got "?<stuff>" This means a call to a Lisp
		  ;; function.  Pass this on to mread which can handle
		  ;; this.
		  ;;
		  ;; Note: There appears to be a bug in Allegro 6.2
		  ;; where concatenated streams don't wait for input
		  ;; on *standard-input*.
		  (mread (make-concatenated-stream 
			  (make-string-input-stream "?") stream)
			 eof-value)))))
	(t
	 (setq *last-dbm-command* nil)
	 (let ((result (mread stream eof-value))
	       (next-char (read-char-no-hang stream eof-error-p eof-value)))
	   (cond
	     ((or (eql next-char nil) (equal next-char '(nil)))
	      (setf *need-prompt* t))
	     ((member next-char '(#\newline #\return))
	      (setf *need-prompt* t))
	     (t
	      (setf *need-prompt* nil)
	      (unread-char next-char stream)))
	   result))))

(defvar *break-level* nil)
(defvar *break-env* nil)
(defvar *top-eof* (cons nil nil))
(defvar *quit-tag* 'macsyma-quit)
;; should maybe be??
;;(defvar *quit-tag* 'si::*quit-tag*)

(defvar *quit-tags* nil)

(defun set-env (bkpt)
  (format *debug-io* 
          (intl:gettext "(~a ~a~@[ in ~a~])") 
          (short-name (bkpt-file bkpt))
	  (bkpt-file-line bkpt)
	  nil)				; (bkpt-function bkpt)
  (format *debug-io* "~&~a:~a::~%" (bkpt-file bkpt)
	  (bkpt-file-line bkpt)))

(defvar *diff-mspeclist* nil)
(defvar *diff-bindlist* nil)

(defun break-dbm-loop (at)
  (let* ((*quit-tags* (cons (cons *break-level* *quit-tag*) *quit-tags*))
	 (*break-level* (if (not at) *break-level* (cons t *break-level*)))
	 (*quit-tag* (cons nil nil))
	 (*break-env* *break-env*)
	 (*mread-prompt* "")
	 (*diff-bindlist* nil)
	 (*diff-mspeclist* nil)
	 val)
    (declare (special *mread-prompt*))
    (and (consp at) (set-env at))
    (cond ((null at)
	   (break-frame 0 nil)))
    (catch 'step-continue
      (catch *quit-tag*
	(unwind-protect
	     (do () (())
	       (format *debug-io*
		       "~a~&~@[(~a:~a) ~]~a"  *prompt-prefix* 
		       (unless (stringp at) "dbm")
		       (length *quit-tags*) *prompt-suffix*)
	       (finish-output *debug-io*)
	       (setq val
		     (catch 'macsyma-quit
		       (let ((res (dbm-read *debug-io*  nil *top-eof* t)))
			 (declare (special *mread-prompt*))
			 (cond ((and (consp res) (keywordp (car res)))
				(let ((value (break-call (car res)
							 (cdr res) 
				                         'break-command)))
				  (cond ((eq value :resume) (return)))))
			       ((eq res *top-eof*)
			        (funcall (get :top 'break-command)))
			       (t
				(setq $__ (nth 2 res))
				(setq $% (meval* $__))
				(setq $_ $__)
				(displa $%)))
			 nil)))
	       (and (eql val 'top)
		    (throw-macsyma-top)))
	  (restore-bindings))))))

(defun break-quit (&optional (level 0)
                   &aux (current-level (length *break-level*)))
  (when (and (>= level 0) (< level current-level))
    (let ((x (nth (- current-level level 1) *quit-tags*)))
      (if (eq (cdr x) 'macsyma-quit)
	  (throw 'macsyma-quit 'top)
	  (throw (cdr x) (cdr x)))))
  (throw 'macsyma-quit 'top))

(defun break-current ()
  (if *break-level*
      (format *debug-io* 
              (intl:gettext "Back to level ~:@(~S~).") 
              (length *break-level*))
      (format *debug-io* (intl:gettext "~&Top level.")))
  (values))

(defun def-break (keyword fun doc)
  (setf (get keyword 'break-command) fun)
  (and doc (setf (get keyword 'break-doc) doc)))

(defun break-help (&optional key)
  (cond (key
	 (if (keywordp key)
	     (dolist (v (complete-prop key 'keyword 'break-doc t))
	       (format t "~&~%~(~s~)   ~a" v (get v 'break-doc)))))
	(t
	 (loop for vv being the symbols of 'keyword
		when (get vv 'break-command)
		collect (cons vv (or (get vv 'break-doc) "Undocumented"))
		into all
		finally (setq all (sort all 'alphalessp))
	      (format t (intl:gettext "~
Break commands start with ':'. Any unique substring may be used,~%~
eg :r :re :res all work for :resume.~2%~
Command      Description~%~
-----------  --------------------------------------"))
		(loop for vv in all
		   do (format t "~% ~(~12s~)" (car vv))
		     (format t (cdr vv)))
	        (finish-output)))))

(def-break :help 'break-help
  "Print help on a break command or with no arguments on
             all break commands")

;; What is this debug command for?
(def-break :_none #'(lambda()) nil)

(def-break :next  'step-next
  "Like :step, except that subroutine calls are stepped over")

(def-break :step  'step-into
  "Step program until it reaches a new source line")

;;(def-break :location  'loc "" )

(def-break :quit 'break-quit 
  "Quit this level")

(def-break :top  #'(lambda( &rest l)l (throw 'macsyma-quit 'top)) 
  "Throw to top level")

(defstruct (line-info (:type list)) line file)

(defstruct (bkpt (:type list)) form file file-line function)

(defun *break-points* (form)
  (let ((pos(position form *break-points* :key 'car)))
    (format t "Bkpt ~a:" pos)
    (break-dbm-loop  (aref *break-points* pos))))

;; fun = function name eg '$|odeSeriesSolve| and 
;; li  = offset from beginning of function. 
;; Or fun = string (filename) and li = absolute position.

(defun break-function (fun &optional (li 0) absolute 
                           &aux i tem info form fun-line)
  (unless *mdebug*
    (format t "~&Turning on debugging debugmode(true)")
    (setq *mdebug* t))
  (cond ((stringp fun)
	 (let ((file fun)  start)
	   (loop named joe for vv being the symbols of 'maxima with tem with linfo
		  when (and (typep (setq tem (set-full-lineinfo vv))
				   'vector)
			    (setq linfo (get-lineinfo (aref tem 1)))
			    (equal file (cadr linfo))
			    (fb >= li (setq start (aref tem 0)))
			    (fb <= li (+ start (length (the vector tem)))))
		  do (setq fun vv li (f- li start -1))
				; (print (list 'found fun fun li  (aref tem 0)))
		  (return-from joe nil)
		  finally
		  (format t "No line info for ~a " fun)
		  (return-from break-function nil)))))
  (setq fun ($concat fun))
					; (print (list 'fun fun 'hi))
  (cond ((and (setq tem (second (mgetl  fun '(mexpr mmacro))))
	      (setq info (get-lineinfo (setq form (third tem))))
	      (eq (third info) 'src))
	 (setq fun-line (fifth info))
	 (or (fixnump fun-line) (setq fun-line (line-info-line info)))
					; (print (list 'fun-line fun-line))
	 (setq form (first-form-line
		     form
		     (setq i (+
			      (if absolute 0 fun-line) li))))
	 (unless form
	   (if (eql li 0)
	       (return-from break-function (break-function fun 1)))
           (format t "~& No instructions recorded for this line ~a of ~a" li
		   ($sconcat fun))
	   (return-from break-function nil))
	 (let ((n (insert-break-point    (make-bkpt :form form
						    :file-line i
						    :file (line-info-file info)
						    :function fun))))
	   (format t "~&Bkpt ~a for ~a (in ~a line ~a) ~%"
		   n ($sconcat fun) (line-info-file info) i)
	   n))
	(t (format t "No line info for ~a " fun))))

;; note  need to make the redefine function, fixup the break point list..

(defun make-break-point (fun ar i)
  (declare (fixnum i) (type (vector t) ar))
  (let* ((tem (aref ar i))
	 (linfo (get-lineinfo tem)))
    (and linfo (list tem (cadr linfo) (car linfo) fun))))

(defun dbm-up (n &aux (cur *current-frame*) (m (length *mlambda-call-stack*)))
  (declare (fixnum n m cur))
  (setq m (quotient m 5))
  (setq n (f + n cur))
  (cond ((fb > n m)
	 (setq n m))
	((fb < n 0)
	 (setq n 0)))
  (break-frame  n nil))

(defun insert-break-point (bpt &aux at)
  (or *break-points* (init-break-points))
  (setq at (or (position nil *break-points*)
	       (prog1 (length *break-points*)
		 (vector-push-extend  nil *break-points*))))
  (let ((fun (bkpt-function bpt)))
    (push at (get fun 'break-points)))
  (setf (aref *break-points* at) bpt)
  at)

(defun short-name (name)
  (let ((pos (position #\/ name :from-end t)))
    (if pos (subseq name (f + 1 pos)) name)))

(defun show-break-point (n &aux disabled)
  (let ((bpt (aref *break-points* n)))
    (when bpt
      (when (eq (car bpt) nil)
	(setq disabled t)
	(setq bpt (cdr bpt)))
      (format t "Bkpt ~a:(~a line ~a)~@[(disabled)~]"
	      n (short-name (second bpt))
	      (third bpt) disabled)
      (let ((fun (fourth bpt)))
	(format t "(line ~a of ~a)"  (relative-line fun (nth 2 bpt))
		fun)))))

(defun relative-line (fun l)
  (let ((info (set-full-lineinfo fun)))
    (if info (f - l (aref info 0))
	0)))

(defun iterate-over-bkpts (l action)
  (dotimes (i (length *break-points*))
    (if (or (member i l)
	    (null l))
	(let ((tem (aref *break-points* i)))
	  (setf (aref *break-points* i)
		(case action
		  (:delete
		   (unless (car tem)
		     (pop tem))	    ; disabled or already deleted bkpt
		   (if tem 
		       (setf (get (bkpt-function tem) 'break-points)
		             (delete i 
		                     (get (bkpt-function tem) 'break-points))))
		   nil)
		  (:enable
		   (if (eq (car tem) nil) (cdr tem) tem))
		  (:disable
		   (if (and tem (not (eq (car tem) nil)))
		       (cons nil tem)
		       tem))
		  (:show
		   (when tem (show-break-point i)
			 (terpri))
		   tem)))))))

;; get the most recent function on the stack with step info.

(defun current-step-fun ( &aux fun)
  (loop for i below 100000
	 while (setq fun (frame-info i))
	 do (cond ((and (symbolp fun) (set-full-lineinfo fun))
		   (return-from current-step-fun fun)))))

(def-break :bt '$backtrace "Print a backtrace of the stack frames")

(def-break :info #'(lambda (&optional type)
		     (case type
		       (:bkpt  (iterate-over-bkpts nil :show)(values))
		       (otherwise
			(format *debug-io* 
                                "usage: :info :bkpt -- show breakpoints")))) 
  "Print information about item")

(defmacro lisp-quiet (&rest l)
  (setq *mread-prompt* "")
  (eval (cons 'progn l)))

(def-break :lisp-quiet 'lisp-quiet 
  "Evaluate the lisp form without printing a prompt")

(def-break :lisp 'lisp-eval 
  "Evaluate the lisp form following on the line")

(defmacro lisp-eval (&rest l)
  (dolist (v (multiple-value-list (eval (cons 'progn l))))
    (fresh-line *standard-output*)
    (princ v)))
   
(def-break :delete #'(lambda (&rest l) (iterate-over-bkpts l :delete) (values))
  "Delete all breakpoints, or if arguments are supplied delete the
             specified breakpoints")

(def-break :frame 'break-frame
  "With an argument print the selected stack frame.
             Otherwise the current frame.")

(def-break :resume  #'(lambda () :resume) 
  "Continue the computation.")

(def-break :continue #'(lambda () :resume)  
  "Continue the computation.")

(def-break :disable 
    #'(lambda (&rest l) (iterate-over-bkpts l :disable)(values))
  "Disable the specified breakpoints, or all if none are specified")

(def-break :enable #'(lambda (&rest l) (iterate-over-bkpts l :enable)(values))
  "Enable the specified breakpoints, or all if none are specified")

(def-break :break 'do-break
  "Set a breakpoint in the specified FUNCTION at the
             specified LINE offset from the beginning of the function.
             If FUNCTION is given as a string, then it is presumed to be
             a FILE and LINE is the offset from the beginning of the file.")

;; force the rest of the line to be broken at spaces,
;; and each item read as a maxima atom.
(setf (get :break 'maxima-read) t)

(defmacro do-break (&optional name &rest l)
  (declare (special *last-dbl-break*))
  (cond ((null name)
	 (if *last-dbl-break*
	     (let ((fun  (nth 3 *last-dbl-break*)))
	       (break-function fun (nth 2 *last-dbl-break*) t))))
	(t (eval `(break-function ',name ,@l)))))

;; this just sets up a counter for each stream.. we want
;; it to start at one.

(defun get-lineinfo (form)
  (cond ((consp form)
	 (if (consp (cadar form))
	     (cadar form)
	     (if (consp (caddar form))
		 (caddar form)
		 nil)))
	(t nil)))

;; restore-bindings from an original binding list.
(defun restore-bindings ()
  (mbind *diff-bindlist* *diff-mspeclist* nil)
  (setf *diff-bindlist* nil *diff-mspeclist* nil))

(defun remove-bindings (the-bindlist)
  (loop for v on bindlist with var
	 while v
	 until (eq v the-bindlist)
	 do
	 (setq var (car v))
	 (push var *diff-bindlist*)
	 (push (symbol-value var) *diff-mspeclist*)
	 (cond ((eq (car mspeclist) munbound)
		(makunbound var) 
	        (setq $values (delete var $values :count 1 :test #'eq)))
	       (t (let ((munbindp t)) (mset var (car mspeclist)))))
	 (setq mspeclist (cdr mspeclist) bindlist (cdr bindlist))))

(defun break-frame (&optional (n 0) (print-frame-number t))
  (restore-bindings)
  (multiple-value-bind (fname vals params backtr lineinfo bdlist)
      (print-one-frame n print-frame-number)
    backtr params vals fname
    (remove-bindings bdlist)
    (when lineinfo
      (fresh-line *debug-io*)
      (format *debug-io* "~a:~a::~%" (cadr lineinfo) (+ 0 (car lineinfo))))
    (values)))
