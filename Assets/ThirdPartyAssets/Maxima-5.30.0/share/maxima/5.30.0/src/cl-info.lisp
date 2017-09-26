(in-package :cl-info)

(defvar *info-tables* (make-hash-table :test 'equal))

(defvar *prompt-prefix* "")
(defvar *prompt-suffix* "")

(defun print-prompt (prompt-count)
  (format t "~&~a~a~a"
	  *prompt-prefix*
	  (if (zerop prompt-count)
	      (intl:gettext "Enter space-separated numbers, `all' or `none': ")
	      (intl:gettext "Still waiting: "))
	  *prompt-suffix*))

(defvar +select-by-keyword-alist+
  '((noop "") (all "a" "al" "all") (none "n" "no" "non" "none")))

(defun parse-user-choice (nitems)
  (loop
   with line = (read-line) and nth and pos = 0
   while (multiple-value-setq (nth pos)
	   (parse-integer line :start pos :junk-allowed t))
   if (or (minusp nth) (>= nth nitems))
   do (format *debug-io* (intl:gettext "~&Discarding invalid number ~d.") nth)
   else collect nth into list
   finally
   (let ((keyword
	  (car (rassoc
		(string-right-trim
		 '(#\space #\tab #\newline #\;) (subseq line pos))
		+select-by-keyword-alist+
		:test #'(lambda (item list)
			  (member item list :test #'string-equal))))))
     (unless keyword
       (setq keyword 'noop)
       (format *debug-io* (intl:gettext "~&Ignoring trailing garbage in input.")))
     (return (cons keyword list)))))

(defun select-info-items (selection items)
  (case (pop selection)
    (noop (loop
	   for i in selection
	   collect (nth i items)))
    (all items)
    (none 'none)))

; ------------------------------------------------------------------
; STUFF ABOVE SALVAGED FROM PREVIOUS INCARNATION OF SRC/CL-INFO.LISP
; STUFF BELOW IS NEW, BASED ON LOOKUP TABLE BUILT AHEAD OF TIME
; ------------------------------------------------------------------

; ------------------ search help topics ------------------

(defun load-primary-index ()
  ;; Load the index, but make sure we use a sensible *read-base*.
  ;; See bug 1951964.  GCL doesn't seem to have
  ;; with-standard-io-syntax.  Is just binding *read-base* enough?  Is
  ;; with-standard-io-syntax too much for what we want?
  (let*
    ((subdir-bit (or maxima::*maxima-lang-subdir* "."))
     (path-to-index (maxima::combine-path maxima::*maxima-infodir* subdir-bit "maxima-index.lisp")))
    #-gcl
    (with-standard-io-syntax (load path-to-index))
    #+gcl
    (let ((*read-base* 10.)) (load path-to-index))))

(defun info-exact (x)
  (let ((exact-matches (exact-topic-match x)))
    (if (not (some-exact x exact-matches))
      (progn
        (format t (intl:gettext "  No exact match found for topic `~a'.~%  Try `?? ~a' (inexact match) instead.~%~%") x x)
        nil)
      (progn
        (display-items exact-matches)
        (if (some-inexact x (inexact-topic-match x))
          (format t (intl:gettext "  There are also some inexact matches for `~a'.~%  Try `?? ~a' to see them.~%~%") x x))
        t))))

(defun some-exact (x matches)
  (some #'identity (flatten-matches x matches)))

(defun some-inexact (x matches)
  (some #'null (flatten-matches x matches)))

(defun flatten-matches (x matches)
  ;; OH GODS, SPARE YOUR SERVANT FROM YOUR FIERY WRATH ...
  (mapcar #'(lambda (y) (equal y x)) (mapcar #'first (apply #'append (mapcar #'second matches)))))

(defun exact-topic-match (topic)
  (setq topic (regex-sanitize topic))
  (loop for dir-name being the hash-keys of *info-tables*
    collect (list dir-name (exact-topic-match-1 topic dir-name))))

(defun exact-topic-match-1 (topic d)
  (let*
    ((section-table (first (gethash d *info-tables*)))
     (defn-table (second (gethash d *info-tables*)))
     (regex1 (concatenate 'string "^" topic "$"))
     (regex2 (concatenate 'string "^" topic " *<[0-9]+>$")))
    (append
      (find-regex-matches regex1 section-table)
      (find-regex-matches regex1 defn-table)
      (find-regex-matches regex2 section-table)
      (find-regex-matches regex2 defn-table))))

(defun info-inexact (x)
  (let ((inexact-matches (inexact-topic-match x)))
    (when inexact-matches
      (display-items inexact-matches))
    (not (null inexact-matches))))

;; MATCHES looks like ((D1 (I11 I12 I12 ...)) (D2 (I21 I22 I23 ...)))
;; Rearrange it to ((D1 I11) (D1 I12) (D1 I13) ... (D2 I21) (D2 I22) (D2 I23) ...)
(defun rearrange-matches (matches)
  (apply #'append (mapcar #'(lambda (di) (let ((d (first di)) (i (second di))) (mapcar #'(lambda (i1) (list d i1)) i))) matches)))

(defun display-items (items)
  (let*
    ((items-list (rearrange-matches items))
     (nitems (length items-list)))

    (loop for i from 0 for item in items-list do
      (when (> nitems 1)
        (let
          ((heading-title (nth 4 (second item)))
           (item-name (first (second item))))
          (format t "~% ~d: ~a~@[  (~a)~]" i item-name heading-title))))

    (setq wanted
          (if (> nitems 1)
          (loop
           for prompt-count from 0
           thereis (progn
                 (finish-output *debug-io*)
                 (print-prompt prompt-count)
                 (force-output)
                 (clear-input)
                 (select-info-items
                  (parse-user-choice nitems) items-list)))
          items-list))
    (clear-input)
    (finish-output *debug-io*)
    (when (consp wanted)
      (format t "~%")
      (loop for item in wanted
        do (format t "~A~%~%" (read-info-text (first item) (second item)))))))

(defun inexact-topic-match (topic)
  (setq topic (regex-sanitize topic))
  (let ((foo (loop for dir-name being the hash-keys of *info-tables*
    collect (list dir-name (inexact-topic-match-1 topic dir-name)))))
    (remove-if #'(lambda (x) (null (second x))) foo)))

(defun inexact-topic-match-1 (topic d)
  (let*
    ((section-table (first (gethash d *info-tables*)))
     (defn-table (second (gethash d *info-tables*))))
    (append
      (find-regex-matches topic section-table)
      (find-regex-matches topic defn-table))))

(defun regex-sanitize (s)
  "Precede any regex special characters with a backslash."
  (let
    ((L (coerce maxima-nregex::*regex-special-chars* 'list)))

    ; WORK AROUND NREGEX STRANGENESS: CARET (^) IS NOT ON LIST *REGEX-SPECIAL-CHARS*
    ; INSTEAD OF CHANGING NREGEX (WITH POTENTIAL FOR INTRODUCING SUBTLE BUGS)
    ; JUST APPEND CARET TO LIST HERE
    (setq L (cons #\^ L))

    (coerce (apply #'append
                   (mapcar #'(lambda (c) (if (member c L :test #'eq)
					     `(#\\ ,c) `(,c))) (coerce s 'list)))
            'string)))

(defun find-regex-matches (regex-string hashtable)
  (let*
    ((regex (maxima-nregex::regex-compile regex-string :case-sensitive nil))
     (regex-fcn (coerce regex 'function))
     (regex-matches nil))
    (maphash
      #'(lambda (key value)
          (if (funcall regex-fcn key)
            (setq regex-matches (cons `(,key . ,value) regex-matches))
            nil))
      hashtable)
    (stable-sort regex-matches #'string-lessp :key #'car)))

(defun read-info-text (dir-name parameters)
  (let*
    ((value (cdr parameters))
     (filename (car value))
     (byte-offset (cadr value))
     (byte-count (caddr value))
     (text (make-string byte-count))
     (path+filename (make-pathname :directory dir-name :name filename)))
    (with-open-file (in path+filename :direction :input)
      (file-position in byte-offset)
      #+gcl (gcl-read-sequence text in :start 0 :end byte-count)
      #-gcl (read-sequence text in :start 0 :end byte-count))
    text))

#+gcl
(defun gcl-read-sequence (s in &key (start 0) (end nil))
  (dotimes (i (- end start))
    (setf (aref s i) (read-char in))))

; --------------- build help topic indices ---------------

(defun load-info-hashtables (dir-name deffn-defvr-pairs section-pairs)
  (if (and (zerop (length section-pairs)) 
           (zerop (length deffn-defvr-pairs)))
    (format t (intl:gettext "warning: ignoring an empty documentation index in ~M~%") dir-name)
    (destructuring-bind
      (section-hashtable deffn-defvr-hashtable)
      (ensure-info-tables dir-name)
      (mapc #'(lambda (x) (setf (gethash (car x) section-hashtable) (cdr x))) section-pairs)
      (mapc #'(lambda (x) (setf (gethash (car x) deffn-defvr-hashtable) (cdr x))) deffn-defvr-pairs))))

(defun ensure-info-tables (dir-name)
  (or (gethash dir-name *info-tables*)
    (let
      ((t1 (make-hash-table :test 'equal))
       (t2 (make-hash-table :test 'equal)))
      (setf (gethash dir-name *info-tables*) (list t1 t2)))))

