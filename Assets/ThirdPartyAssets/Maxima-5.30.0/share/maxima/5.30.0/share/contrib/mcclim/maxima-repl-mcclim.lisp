;;; Copyright (c) 2005 by Raymond Toy.  License: GPL
;;;
;;; A very gross hack to hook up maxima to mcclim.
;;;
;;; Make sure you have McCLIM and Maxima loaded.  Then compile and
;;; load this file.  Run (clim-user::run-listener) to start the
;;; window.  In the window, you can type typical maxima expressions
;;; and see the output.
;;;
;;; A couple of notes about input:
;;;   o The terminating ";" or "$" will terminate input---you don't need to press enter.
;;;   o If you have a syntax error like mismatched parentheses, the
;;;     parser will indicate it immediately in the gray area at the
;;;     bottom of the screen.  This is actually quite nice.
;;;
;;; One some systems, it seems that the input and output lines (%i and
;;; %o) are clickable and clicking on them will paste them in as if
;;; you had typed the expression.  This needs work, however.

;; Maxima needs to be loaded before this file can work!
(eval-when (:compile-toplevel :load-toplevel)
  (require :maxima))

(in-package :maxima)
;; Replace some of maxima's display routines with own.

(defvar *x-width* nil)
(defvar *y-width* nil)

(defun cursorpos ()
  (multiple-value-bind (x y)
      (clim:stream-cursor-position *standard-output*)
    ;;(format *trace-output* " cursorpos = ~A ~A~%" x y)
    (list (round y *y-width*) (round x *x-width*))))

(defun cursorpos* (row col)
  ;;(format *trace-output* " set row,col = ~A ~A~%" row col)
  (setf (clim::stream-cursor-position *standard-output*)
	(values (* col *x-width*) (* row *y-width*)))
  (setq oldrow row
	oldcol col))

(defun clim-display2d (form &optional (w 0))
  (let ((displayp t)
	(linearray (if displayp (make-array 80.) linearray))
	(mratp (checkrat form))
	(#.writefilep #.writefilep)
	(maxht     1) (maxdp   0) (width   0)
	(height    0) (depth   0) (level   0) (size   2)
	(break     0) (right   0) (lines   1) bkpt
	(bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	(bkptlevel 0) in-p
	(moreflush d-moreflush)
	more-^w
	(moremsg d-moremsg))
    (unwind-protect
	 (progn (setq form (dimension form
				      nil 'mparen 'mparen 0 0))
		(checkbreak form width)
		(clim-output form (if (and (not $leftjust) (= 2 lines))
				      (f- linel (f- width bkptout))
				      0))
		)
      ;; make sure the linearray gets cleared out.
      (clear-linearray))))

;; Set this to #'clim-display2d to use 2D stream I/O instead of just
;; outputing a string.  Otherwise, set it to nil
(setf *alt-display2d* #'clim-display2d)

(defun clim-output (result w)
  (declare (fixnum w))
  ;;(format *trace-output* "stdout = ~A~%" *standard-output*)
  (clim-output-2d (nreverse result) w))

(defun clim-output-2d (result w &aux (h 0))
  (declare (fixnum w h ))
  (setq oldrow (car (cursorpos))
	oldcol 0
	h (+ oldrow bkptht bkptdp))
  (cursorpos* oldrow 0)
  ;; Move the cursor vertically until we are at the bottom line of the
  ;; new expression.
  (do ()
      ((= h oldrow))
    ;;(tyo* #\newline)
    (incf oldrow))
  (cursorpos* oldrow 0)
  (draw-2d result (f- oldrow bkptdp 1) w)
  (cursorpos* (setq h (min (f- ttyheight 2) h)) 0))

(defun draw-2d (dmstr row col)
  (declare (fixnum row col))
  ;;(format *trace-output* "draw-2d at ~A ~A~%" row col)
  (cursorpos* row col)
  (do ((l dmstr))
      ((null l))
    ;;(format *trace-output* "l = ~S~%" l)
    (cond ((characterp (car l))
	   (clim-tyo* (car l))
	   (pop l))
	  ((and (listp (car l))
		(integerp (caar l)))
	   (setq col oldcol)
	   (do ()
	       ((or (characterp (car l))
		    (not (and (listp (car l))
			      (integerp (caar l))))))
	     (cond
	       ((null (cddar l))
		(setq col (+ col (caar l))))
	       (t (draw-2d (reverse (cddar l))
			   (-  row (cadar l)) (+ col (caar l)))
		  (setq col oldcol)))
	     (pop l))
	   (cursorpos* row col))
	  (t
	   ;;(format *trace-output* " T: ~S~%" l)
	   (apply (caar l) nil (cdar l))
	   (pop l)))))

(defun clim-tyo* (char)
  (cond ((char= #\backspace char)
	 (decf oldcol))			;Backspace
	((char< char #.(code-char 128))
	 (incf oldcol)))		;Printing graphic
  ;;(format *trace-output* "  tyo* ~S at ~A~%" char oldcol)
  (princ char))

(defun d-box (linear? h d w body &aux (char 0) dmstr)
					;char a char?
  (declare (fixnum h d w ))
  (cond		     ;((and (not linear?) line-graphics-tty $linedisp)
    ;;	 (let ((x-min (f* lg-character-x oldcol))
    ;;	       (x-max (f* lg-character-x (f+ oldcol w 2)))
    ;;	       (y-min (f+ (f* lg-character-y (f- oldrow h)) 2))
    ;;	       (y-max (f- (f* lg-character-y (f+ oldrow d 2)) 2)))
    ;;	      (declare (fixnum x-min x-max y-min y-max))
    ;;	      (lg-set-point x-min y-min)
    ;;	      (lg-draw-vector x-max y-min)
    ;;	      (lg-draw-vector x-max y-max)
    ;;	      (lg-draw-vector x-min y-max)
    ;;	      (lg-end-vector  x-min y-min))
    ;;	 (cursorpos* oldrow (f1+ oldcol))
    ;;	 (draw-2d body oldrow oldcol)
    ;;	 (cursorpos* oldrow (f+ oldcol 1)))
    ;;	((and (not linear?) character-graphics-tty $linedisp)
    ;;	 (d-matrix nil 'left (f1+ h) (f1+ d))
    ;;	 (cursorpos* (f- oldrow h) oldcol)
    ;;	 (d-hbar nil w)
    ;;	 (cursorpos* (f+ oldrow h) (f- oldcol w))
    ;;	 (draw-2d body oldrow oldcol)
    ;;	 (cursorpos* (f+ oldrow d 1) (f- oldcol w))
    ;;	 (d-hbar nil w)
    ;;	 (cursorpos* (f- oldrow d 1) oldcol)
    ;;	 (d-matrix nil 'right (f1+ h) (f1+ d)))
    (t (setq char #\- #+nil(getcharn $boxchar 2))
       (setq dmstr
	     `((0 ,h (d-hbar ,(f+ 2 w) ,char))
	       (,(f- (f+ w 2)) 0)
	       (d-vbar ,h ,d #\|)
	       ,@body
	       (,(f- (f1+ w)) ,(f- (f1+ d)) (d-hbar ,(f+ w 2) ,char))
	       (-1 0)
	       (d-vbar ,h ,d #\|)))
       (if linear?
	   (draw-linear dmstr oldrow oldcol)
	   (draw-2d dmstr oldrow oldcol)))))


(in-package :clim-user)
;; CLIM needs to be loaded too, of course.

(defparameter *listener-use-debug-io* #+hefner t #-hefner nil)

(defclass showtime-pane (application-pane) ()
  (:default-initargs :background +gray90+))

(define-application-frame maxima-repl (standard-application-frame)
  ((system-command-reader :accessor system-command-reader
			    :initarg :system-command-reader
			    :initform t))
  (:panes
   (interactor :interactor :scroll-bars t)
   (doc :pointer-documentation)
   (showtime (make-pane 'showtime-pane
			:min-height 18
			:display-function 'display-showtime
			:scroll-bars t
			:display-time :command-loop
			:end-of-line-action :allow)))
  (:top-level (default-frame-top-level :prompt 'maxima-prompt))
  (:command-table (maxima-repl
                   :inherit-from (lisp-commands)
		   :menu (("Lisp"        :menu lisp-commands))
		   ))
  (:layouts
   (default
       (vertically ()
	 interactor
	 doc
	 showtime))))


(defun maxima-prompt (stream frame)
  (declare (ignore frame))
  (with-text-face (stream :italic)
    (princ (maxima::main-prompt) stream)))

(defparameter *listener-initial-function* nil)

(defvar *showtime* "")

(defun display-showtime (frame pane)
  (princ *showtime* pane))

(defmethod run-frame-top-level ((frame maxima-repl) &key listener-funcall &allow-other-keys)
  (let ((*debug-io* (if *listener-use-debug-io*
                        (get-frame-pane frame 'interactor)
			*debug-io*))
	;; Borrowed from OpenMCL.
	;; from CLtL2, table 22-7:
        (*listener-initial-function* listener-funcall)
	(*package* *package*)
	(*print-array* *print-array*)
	(*print-base* *print-base*)
	(*print-case* *print-case*)
	(*print-circle* *print-circle*)
	(*print-escape* *print-escape*)
	(*print-gensym* *print-gensym*)
	(*print-length* *print-length*)
	(*print-level* *print-level*)
	(*print-lines* *print-lines*)
	(*print-miser-width* *print-miser-width*)
	(*print-pprint-dispatch* *print-pprint-dispatch*)
	(*print-pretty* *print-pretty*)
	(*print-radix* *print-radix*)
	(*print-readably* *print-readably*)
	(*print-right-margin* *print-right-margin*)
	(*read-base* *read-base*)
	(*read-default-float-format* *read-default-float-format*)
	(*read-eval* *read-eval*)
	(*read-suppress* *read-suppress*)
	(*readtable* *readtable*))    
    (loop while 
      (catch 'return-to-listener
	(restart-case (call-next-method)
	  (return-to-listener ()
	    :report "Return to listener."
	    (throw 'return-to-listener T)))))))


(define-command-table lisp-commands :inherit-from nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type maxima-expression () :inherit-from t)
  )

(define-presentation-method present (object (type maxima-expression)
					    stream (view textual-view)
					    &key &allow-other-keys)
  (cond
    (maxima::*alt-display2d*
     ;; This method has problems with scrolling at the bottom of the
     ;; screen.
     (let ((*standard-output* stream)
	   (maxima::$display2d t)
	   (maxima::*y-width* (text-style-height (medium-text-style stream)
						 stream))
	   (maxima::*x-width* (text-style-width (medium-text-style stream)
						stream)))
       (format *trace-output* "x,y width = ~A ~A~%"
	       maxima::*x-width* maxima::*y-width*)
       (let ((r (with-new-output-record (t)
		  (maxima::displa object))))
	 r)))
    (t
      ;; This method works and scrolling is good.
      (princ (untabify (with-output-to-string (s)
			 (let ((*standard-output* s))
			   (if (eq (caar object) 'maxima::displayinput)
			       (let ((maxima::$display2d nil))
				 (maxima::displa `((maxima::mlabel) nil ,@(cddr object))))
			       (maxima::displa object)))))
	     stream))))


(define-presentation-method accept ((type maxima-expression)
				    stream (view textual-view)
				    &key)
  (maxima::dbm-read stream nil stream))

(defvar *boxify* nil)

(define-command (com-eval :menu t :command-table lisp-commands)
    ((form 'clim:form :prompt "form"))
  (let ((r form)
	(time-before)
	(time-after)
	(time-used)
	(eof (list nil))
	(etime-before)
	(etime-after)
	(area-before)
	(area-after)
	(etime-used)
	(maxima::c-tag)
	(maxima::d-tag))

    (setq maxima::c-tag (maxima::makelabel maxima::$inchar))
    (setq maxima::$__ (caddr r))
    (set  maxima::c-tag maxima::$__)
    (setq time-before (get-internal-run-time)
	  etime-before (get-internal-real-time))
    (setq area-before (maxima::used-area))

    (setq maxima::$% (maxima::toplevel-macsyma-eval maxima::$__))

    (setq etime-after (get-internal-real-time)
	  time-after (get-internal-run-time))
    (setq area-after (maxima::used-area))
    (setq time-used (maxima::quotient 
		     (float (maxima::difference time-after time-before))
		     internal-time-units-per-second)
	  etime-used (maxima::quotient 
		      (float (maxima::difference etime-after etime-before))
		      internal-time-units-per-second))
    (setq maxima::accumulated-time (maxima::plus maxima::accumulated-time time-used))
    (set (setq maxima::d-tag (maxima::makelabel maxima::$outchar)) maxima::$%)
    (setq maxima::$_ maxima::$__)
    (setq *showtime*
	  (with-output-to-string (s)
	    (format s "Evaluation took ~$ seconds (~$ elapsed)"
		    time-used etime-used )
	    #+(or cmu sbcl clisp)
	    (let ((total-bytes (- area-after area-before)))
	      (cond ((> total-bytes (* 1024 1024))
		     (format s " using ~,3F MB.~%"
			     (/ total-bytes (* 1024.0 1024.0))))
		    ((> total-bytes 1024)
		     (format s " using ~,3F KB.~%" (/ total-bytes 1024.0)))
		    (t
		     (format s " using ~:D bytes.~%" total-bytes))))))
    (unless maxima::$nolabels
      (maxima::putprop maxima::d-tag (cons time-used  0) 'maxima::time))

    (when (not (maxima::checklabel maxima::$inchar))
      (setq maxima::$linenum (1+ maxima::$linenum)))

    ;; I don't know why I need to do this, but with my showtime pane
    ;; enabled, *standard-output* is set to the showtime-pane so the
    ;; output here goes to the wrong place.  This sets
    ;; *standard-output* to the interactor pane that we want.
    (let ((*standard-output* (get-frame-pane *application-frame* 'interactor)))
      (with-drawing-options (t :ink +olivedrab+)
	(present `((maxima::mlabel) nil ,(if *boxify*
					     (maxima::boxify maxima::$%)
					     maxima::$%))
		 'maxima-expression)))))


(defmethod read-frame-command ((frame maxima-repl) &key (stream *standard-input*))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (let (object)
    (with-input-editing (stream :input-sensitizer
				(lambda (stream cont)
				  (with-output-as-presentation 
				      (stream object 'maxima-expression)
				    (funcall cont))))
      (setq object (accept 'maxima-expression :stream stream :prompt nil
			   :activation-gestures nil))
      (list 'com-eval object))))


#+nil
(defmethod read-frame-command :around ((frame maxima-repl)
				       &key (stream *standard-input*))
  "Read a command or form, taking care to manage the input context
   and whatever else need be done."
  (multiple-value-bind (x y)
      (stream-cursor-position stream)    
    (with-input-context ('command) (object object-type)
            (call-next-method)
        (command
         ;; Kludge the cursor position - Goatee will have moved it all around
         (setf (stream-cursor-position stream) (values x y))
         (present object object-type
                  :view (stream-default-view stream)
                  :stream stream)
         object))))

(defclass redisplay-frame-mixin ()
  ())

(defmethod redisplay-frame-pane :after
    ((frame redisplay-frame-mixin) (pane application-pane) &key force-p)
  (declare (ignore force-p))
  (change-space-requirements
   pane :height (bounding-rectangle-height (stream-output-history pane))))

(defun run-listener (&key (system-command-reader nil)
                          (new-process nil)
                          (width 760)
                          (height 550)
                          (process-name "Listener")
                          (eval nil))
  (flet ((run ()
           (run-frame-top-level
            (make-application-frame 'maxima-repl
                                    :width width
                                    :height height
                                    :system-command-reader system-command-reader)
            :listener-funcall (cond ((null eval) nil)
                                    ((functionp eval) eval)
                                    (t (lambda () (eval eval)))))))
    (let ((*package* (find-package "MAXIMA")))
    (if new-process
        (clim-sys:make-process #'run :name process-name)
        (run)))))

(defun untabify (input)
  (let ((line-posn 0))
    (with-output-to-string (output)
      (dotimes (i (length input))
	(let ((char (char input i)))
	  (case char
	    (#\Tab
	     (let ((spaces (- 8 (rem line-posn 8))))
	       (dotimes (k spaces)
		 (incf line-posn)
		 (write-char #\space output))))
	    (#\Newline
	     (setf line-posn 0)
	     (write-char #\Newline output))
	    (t
	     (incf line-posn)
	     (write-char char output))))))))


(in-package :maxima)
(defun clim-display (form &rest args)
  (let ((displayp t)
	(linearray (if displayp (make-array 80.) linearray))
	(mratp (checkrat form))
	(#.writefilep #.writefilep)
	(maxht     1) (maxdp   0) (width   0)
	(height    0) (depth   0) (level   0) (size   2)
	(break     0) (right   0) (lines   1) bkpt
	(bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	(bkptlevel 0) in-p
	(moreflush d-moreflush)
	more-^w
	(moremsg d-moremsg))
    (unwind-protect
	 (let ((form (dimension form
				nil 'mparen 'mparen 0 0)))
	   (checkbreak form width)
	   (output form (if (and (not $leftjust) (= 2 lines))
			    (f- linel (f- width bkptout))
			    0)))
      ;; make sure the linearray gets cleared out.
      (clear-linearray))))

(defun convert-expr-to-dimensions (form)
  (let ((displayp t)
	(linearray (if displayp (make-array 80.) linearray))
	(mratp (checkrat form))
	(#.writefilep #.writefilep)
	(maxht     1) (maxdp   0) (width   0)
	(height    0) (depth   0) (level   0) (size   2)
	(break     0) (right   0) (lines   1) bkpt
	(bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	(bkptlevel 0) in-p
	(moreflush d-moreflush)
	more-^w
	(moremsg d-moremsg))
    (unwind-protect
	 (let ((form (dimension form
				nil 'mparen 'mparen 0 0)))
	   (checkbreak form width)
	   form))))

#+nil
(setf maxima::*alt-display2d* #'clim-display)

(in-package :maxima)
(defun boxify (form)
  "Takes a maxima internal form and adds boxes everywhere, as if dpart
  was done on all possible places"
  (labels ((boxify-internal (f)
	   (cond ((atom f)
		  `((mbox simp) ,f))
		 ((eq (caar f) 'rat)
		  ;; We have ((rat) n m).  Convert to ((mtimes) n ((mexpt) m -1)).
		  (destructuring-bind (r n m)
		      f
		    (declare (ignore r))
		    `((mtimes simp) ,(if (= n -1) -1
					 (boxify-internal n))
		      ((mexpt simp) ,(boxify-internal m) -1))))
		 ((and (eq (caar f) 'mplus)
		       (eql (second f) -1))
		  `(,(car f) -1 ,@(mapcar #'boxify-internal (cddr f))))
		 ((eq (caar f) 'mtimes)
		  (destructuring-bind (r a &rest b)
		      f
		    (cond ((eql a -1)
			   ;; Need to handle multiplication by -1 specially.
			   `(,r ,a ,@(mapcar #'boxify-internal b)))
			  ((and (listp a)
				(eq (caar a) 'rat)
				(or (eql (second a) 1)
				    (eql (second a) -1)))
			   ;; Handle multiplication by 1/n or -1/n specially
			   `(,r ,a ,@(mapcar #'boxify-internal b)))
			  (t
			   `(,r ,@(mapcar #'boxify-internal (cdr f)))))))
		 ((and (eq (caar f) 'mexpt)
		       (= (length f) 3)
		       (or (alike1 (third f) 1//2)
			   (alike1 (third f) -1//2)))
		  (cond ((alike1 (third f) 1//2)
			;; Handle sqrt specially
			 `((mbox simp) (,(car f) ,(boxify-internal (second f)) ,(third f))))
			((alike1 (third f) -1//2)
			 `((mexpt simp) ((mbox simp) (,(car f)
					              ,(boxify-internal (second f)) 
					              ((rat simp) 1 2)))
			   -1))))
		 (t
		  `((mbox simp) (,(car f) ,@(mapcar #'boxify-internal (cdr f))))))))
    (if (listp form)
	;;`(,(car form) ,@(mapcar #'boxify-internal (cdr form)))
	(boxify-internal form)
	form)))
