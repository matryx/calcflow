;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: INTL -*-

;;; $Revision: 1.16 $
;;; Copyright 1999-2010 Paul Foley (mycroft@actrix.gen.nz)
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this Software to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; provided that the above copyright notice and this permission notice
;;; are included in all copies or substantial portions of the Software.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.
(in-package :intl)

(eval-when #-gcl (:compile-toplevel :execute)
	   #+gcl (compile eval)
  (defparameter intl::*default-domain* "maxima")
  (unless (and (fboundp 'intl:read-translatable-string)
	       (eq (get-macro-character #\_)
		   (fdefinition 'intl:read-translatable-string)))
    (set-macro-character #\_ (lambda (stream char)
			       (declare (ignore char))
			       (case (peek-char nil stream nil nil t)
				 (#\" (values))
				 (#\N (read-char stream t nil t) (values))
				 (otherwise '_)))
			 t)))
(defvar *locale-directories* '(#p"/usr/share/locale/"))
(defvar *locale* "C")

(defvar *default-domain* "maxima"
  _N"The message-lookup domain used by INTL:GETTEXT and INTL:NGETTEXT.
  Use (INTL:TEXTDOMAIN \"whatever\") in each source file to set this.")
(defvar *loaded-domains* (make-hash-table :test 'equal))
(defvar *locale-aliases* (make-hash-table :test 'equal))

(defstruct domain-entry
  (domain "" :type simple-string)
  (locale "" :type simple-string)
  (file #p"" :type pathname)
  (plurals nil :type (or null function))
  (hash (make-hash-table :test 'equal) :type hash-table)
  (encoding nil)
  (readfn #'identity :type function))

(declaim (ftype (function (stream) (unsigned-byte 32)) read-lelong))
(defun read-lelong (stream)
  (declare (optimize (speed 3) (space 2)
		     #+CMU (ext:inhibit-warnings 3))) ;quiet about boxing retn
  (+ (the (unsigned-byte 8) (read-byte stream))
     (ash (the (unsigned-byte 8) (read-byte stream)) 8)
     (ash (the (unsigned-byte 8) (read-byte stream)) 16)
     (ash (the (unsigned-byte 8) (read-byte stream)) 24)))

(declaim (ftype (function (stream) (unsigned-byte 32)) read-belong))
(defun read-belong (stream)
  (declare (optimize (speed 3) (space 2)
		     #+CMU (ext:inhibit-warnings 3))) ;quiet about boxing retn
  (+ (ash (the (unsigned-byte 8) (read-byte stream)) 24)
     (ash (the (unsigned-byte 8) (read-byte stream)) 16)
     (ash (the (unsigned-byte 8) (read-byte stream)) 8)
     (the (unsigned-byte 8) (read-byte stream))))

(defun locate-domain-file (domain locale locale-dir)
  (flet ((path (locale base)
	   (merge-pathnames (make-pathname :directory (list :relative locale
							    "LC_MESSAGES")
					   :name domain :type "mo")
			    base)))
    (let ((locale (or (gethash locale *locale-aliases*) locale)))
      (dolist (base (if (listp locale-dir) locale-dir (list locale-dir)))
	(let ((probe
	       (or (probe-file (path locale base))
		   (let ((dot (position #\. locale)))
		     (and dot (probe-file (path (subseq locale 0 dot) base))))
		   (let ((at (position #\@ locale)))
		     (and at (probe-file (path (subseq locale 0 at) base))))
		   (let ((us (position #\_ locale)))
		     (and us (probe-file (path (subseq locale 0 us) base)))))))
	  (when probe (return probe)))))))

(defun find-encoding (domain)
  (when (null (domain-entry-encoding domain))
    (setf (domain-entry-encoding domain) :iso-8859-1)
    (let* ((header (domain-lookup "" domain))
	   (ctype (search "Content-Type: " header))
	   (eoln (and ctype (position #\Newline header :start ctype)))
	   (charset (and ctype (search "; charset=" header
				       :start2 ctype :end2 eoln))))
      (when charset
	(incf charset 10)
	(loop for i upfrom charset below eoln as c = (char header i)
            while (or (alphanumericp c) (eql c #\-))
          finally (setf (domain-entry-encoding domain)
		      (intern (nstring-upcase (subseq header charset i))
			      :keyword))))))
  domain)

(defun parse-plurals (domain)
  (let* ((header (domain-lookup "" domain))
	 (plurals (search "Plural-Forms: " header))
	 (default (lambda (n) (if (= n 1) 0 1))))
    (if (and plurals
	     (> (length header) (+ plurals 36))
	     (string= header "nplurals="
		      :start1 (+ plurals 14) :end1 (+ plurals 23)))
	(let ((nplurals
	       (parse-integer header :start (+ plurals 23) :junk-allowed t))
	      (point (+ (position #\; header :start (+ plurals 23)) 2)))
	  (if (and (> (length header) (+ point 10))
		   (string= header "plural=" :start1 point :end1 (+ point 7)))
	      (values (parse-expr header (+ point 7)) nplurals)
	      (values default 2)))
	(values default 2))))

(defun parse-expr (string pos)
  (labels ((next ()
	     (loop while (member (char string pos) '(#\Space #\Tab #\Newline))
		   do (incf pos))
	     (case (char string (1- (incf pos)))
	       (#\n 'n)
	       (#\? 'if)
	       (#\: 'then)
	       (#\( 'lpar)
	       (#\) 'rpar)
	       (#\^ 'logxor)
	       (#\+ 'add)
	       (#\- 'sub)
	       (#\* 'mul)
	       (#\/ 'floor)
	       (#\% 'mod)
	       (#\~ 'lognot32)
	       (#\; 'end)
	       (#\| (if (char= (char string pos) #\|)
			(progn (incf pos) 'cor)
			'logior))
	       (#\& (if (char= (char string pos) #\&)
			(progn (incf pos) 'cand)
			'logand))
	       (#\= (if (char= (char string pos) #\=)
			(progn (incf pos) 'cmp=)
			(error _"Encountered illegal token: =")))
	       (#\! (if (char= (char string pos) #\=)
			(progn (incf pos) 'cmp/=)
			'not))
	       (#\< (case (char string pos)
		      (#\= (incf pos) 'cmp<=)
		      (#\< (incf pos) 'shl)
		      (otherwise 'cmp<)))
	       (#\> (case (char string pos)
		      (#\= (incf pos) 'cmp>=)
		      (#\> (incf pos) 'shr)
		      (otherwise 'cmp>)))
	       (otherwise (let ((n (digit-char-p (char string (1- pos)))))
			    (if n
				(loop for nx = (digit-char-p (char string pos))
				      while nx
				   do (setq n (+ (* n 10) nx)) (incf pos)
				   finally (return n))
				(error _"Encountered illegal token: ~C"
				       (char string (1- pos))))))))
	   (conditional (tok &aux tree)
	     (multiple-value-setq (tree tok) (logical-or tok))
	     (when (eql tok 'if)
	       (multiple-value-bind (right next) (logical-or (next))
		 (unless (eql next 'then)
		   (error _"Expected : in ?: construct"))
		 (multiple-value-bind (else next) (conditional (next))
		   (setq tree (list tok (list 'zerop tree) else right)
			 tok next))))
	     (values tree tok))
	   (logical-or (tok &aux tree)
	     (multiple-value-setq (tree tok) (logical-and tok))
	     (loop while (eql tok 'cor) do
		(multiple-value-bind (right next) (logical-and (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (logical-and (tok &aux tree)
	     (multiple-value-setq (tree tok) (inclusive-or tok))
	     (loop while (eql tok 'cand) do
		(multiple-value-bind (right next) (inclusive-or (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (inclusive-or (tok &aux tree)
	     (multiple-value-setq (tree tok) (exclusive-or tok))
	     (loop while (eql tok 'logior) do
		(multiple-value-bind (right next) (exclusive-or (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (exclusive-or (tok &aux tree)
	     (multiple-value-setq (tree tok) (bitwise-and tok))
	     (loop while (eql tok 'logxor) do
		(multiple-value-bind (right next) (bitwise-and (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (bitwise-and (tok &aux tree)
	     (multiple-value-setq (tree tok) (equality tok))
	     (loop while (eql tok 'logand) do
		(multiple-value-bind (right next) (equality (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (equality (tok &aux tree)
	     (multiple-value-setq (tree tok) (relational tok))
	     (loop while (member tok '(cmp= cmp/=)) do
		(multiple-value-bind (right next) (relational (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (relational (tok &aux tree)
	     (multiple-value-setq (tree tok) (shift tok))
	     (loop while (member tok '(cmp< cmp> cmp<= cmp>=)) do
		(multiple-value-bind (right next) (shift (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (shift (tok &aux tree)
	     (multiple-value-setq (tree tok) (additive tok))
	     (loop while (member tok '(shl shr)) do
		(multiple-value-bind (right next) (additive (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (additive (tok &aux tree)
	     (multiple-value-setq (tree tok) (multiplicative tok))
	     (loop while (member tok '(add sub)) do
		(multiple-value-bind (right next) (multiplicative (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (multiplicative (tok &aux tree)
	     (multiple-value-setq (tree tok) (unary tok))
	     (loop while (member tok '(mul floor mod)) do
		(multiple-value-bind (right next) (unary (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (unary (tok &aux tree)
	     (cond ((eq tok 'lpar)
		    (multiple-value-setq (tree tok) (conditional (next)))
		    (unless (eq tok 'rpar)
		      (error _"Expected close-paren."))
		    (values tree (next)))
		   ((numberp tok)
		    (values tok (next)))
		   ((eql tok 'n)
		    (values tok (next)))
		   ((eql tok 'add)
		    (unary (next)))
		   ((eql tok 'sub)
		    (multiple-value-setq (tree tok) (unary (next)))
		    (values (list '- tree) tok))
		   ((eql tok 'lognot32)
		    (multiple-value-setq (tree tok) (unary (next)))
		    (values (list 'lognot32 tree) tok))
		   ((eql tok 'not)
		    (multiple-value-setq (tree tok) (unary (next)))
		    (values (list 'cnot tree) tok))
		   (t
		    (error _"Unexpected token: ~S." tok)))))
    (multiple-value-bind (tree end) (conditional (next))
      (unless (eq end 'end)
	(error _"Expecting end of expression.  ~S." end))
      (let (#-gcl
	    (*compile-print* nil))
	(compile nil
		 `(lambda (n)
		    (declare (type (unsigned-byte 32) n)
			     (optimize (space 3)))
		    (flet ((add   (a b) (ldb (byte 32 0) (+ a b)))
			   (sub   (a b) (ldb (byte 32 0) (- a b)))
			   (mul   (a b) (ldb (byte 32 0) (* a b)))
			   (shl   (a b) (ldb (byte 32 0) (ash a b)))
			   (shr   (a b) (ash a (- b)))
			   (cmp=  (a b) (if (= a b) 1 0))
			   (cmp/= (a b) (if (/= a b) 1 0))
			   (cmp<  (a b) (if (< a b) 1 0))
			   (cmp<= (a b) (if (<= a b) 1 0))
			   (cmp>  (a b) (if (> a b) 1 0))
			   (cmp>= (a b) (if (>= a b) 1 0))
			   (cand  (a b) (if (or (zerop a) (zerop b)) 0 1))
			   (cor   (a b) (if (and (zerop a) (zerop b)) 0 1))
			   (cnot  (a)   (if a 0 1))
			   (lognot32 (a) (ldb (byte 32 0) (lognot a))))
		      (declare (ignorable #'add #'sub #'mul #'shr #'shl
					  #'cmp= #'cmp/=
					  #'cmp< #'cmp<= #'cmp> #'cmp>=
					  #'cand #'cor #'cnot #'lognot32))
		      ,tree)))))))

(defun load-domain (domain locale &optional (locale-dir *locale-directories*))
  (let ((file (locate-domain-file domain locale locale-dir))
	(read #'read-lelong))
    (unless file (return-from load-domain nil))
    (with-open-file (stream file :direction :input :if-does-not-exist nil
			    :element-type '(unsigned-byte 8))
      (unless stream (return-from load-domain nil))
      (let ((magic (read-lelong stream)))
	(cond ((= magic #x950412de) (setq read #'read-lelong))
	      ((= magic #xde120495) (setq read #'read-belong))
	      (t
	       (error _"Bad magic number in \"~A.mo\"." domain))))
      (let ((version (funcall read stream))
	    (messages (funcall read stream))
	    (master (funcall read stream))
	    (translation (funcall read stream))
	    (entry (make-domain-entry)))
	(declare (ignore version))
	(setf (domain-entry-readfn entry) read)
	(setf (domain-entry-domain entry) domain)
	(setf (domain-entry-locale entry) locale)
	(setf (domain-entry-file entry) file)
	(dotimes (msg messages)
	  (file-position stream (+ master (* 8 msg)))
	  (let ((length (funcall read stream))
		(start (funcall read stream)))
	    (setf (gethash length (domain-entry-hash entry))
		  (acons start (+ translation (* 8 msg))
			 (gethash length (domain-entry-hash entry))))))
	(setf (gethash domain *loaded-domains*) entry)
	(find-encoding entry)))))

(defun find-domain (domain locale &optional (locale-dir *locale-directories*))
  (let ((found (gethash domain *loaded-domains*)))
    (if (and found (string= (domain-entry-locale found) locale))
	found
	(load-domain domain locale locale-dir))))

(declaim (inline string-to-octets))
(defun string-to-octets (string encoding)
  (declare (ignorable encoding))
  #+(and CMU Unicode)
  (ext:string-to-octets string :external-format encoding)
  #+scl
  (ext:make-bytes-from-string string encoding)
  #+Allegro
  (excl:string-to-octets string :external-format encoding :null-terminate nil)
  #+SBCL
  (sb-ext:string-to-octets string :external-format encoding
			   :null-terminate nil)
  #+CLISP
  (ext:convert-string-to-bytes string (ext:make-encoding :charset (symbol-name encoding)))
  ;;@@ add other implementations
  #-(or (and CMU Unicode) Allegro SBCL CLISP scl #|others|#)
  (map-into (make-array (length string) :element-type '(unsigned-byte 8))
	    #'char-code string))

(declaim (inline octets-to-string))
(defun octets-to-string (octets encoding)
  (declare (ignorable encoding))
  #+(and CMU Unicode)
  (ext:octets-to-string octets :external-format encoding)
  #+scl
  (ext:make-string-from-bytes octets encoding)
  #+Allegro
  (excl:octets-to-string octets :external-format encoding :end (length octets))
  #+SBCL
  (sb-ext:octets-to-string octets :external-format encoding)
  #+CLISP ;;@@ Not sure if encoding keyword is OK here
  (ext:convert-string-from-bytes octets (ext:make-encoding :charset (symbol-name encoding)))
  ;;@@ add other implementations
  #-(or (and CMU Unicode) Allegro SBCL CLISP scl #|others|#)
  (map-into (make-string (length octets)) #'code-char octets))

(defun octets= (a b &key (start1 0) (end1 (length a))
			 (start2 0) (end2 (length b)))
  (declare (type (simple-array (unsigned-byte 8) (*)) a b)
	   (type (integer 0 #.array-dimension-limit) start1 end1 start2 end2)
	   (optimize (speed 3) (space 2) #-gcl (debug 0)))
  (when (and (< start1 end1)
	     (< start2 end2))
    (loop
       (unless (= (aref a start1) (aref b start2)) (return nil))
       (when (or (= (incf start1) end1) (= (incf start2) end2)) (return t)))))

(defun search-domain (octets domain pos)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
	   (type domain-entry domain)
	   (type list pos)
	   (optimize (speed 3) (space 2) #-gcl (debug 0)
		     #+CMU (ext:inhibit-warnings 3))) ; quiet about boxing
  (when pos
    (let ((temp (make-array 120 :element-type '(unsigned-byte 8)))
	  (length (length octets)))
      (with-open-file (stream (domain-entry-file domain)
			      :direction :input
			      :element-type '(unsigned-byte 8))
	(dolist (entry pos)
	  (file-position stream (car entry))
	  (let ((off 0)
		(end (read-sequence temp stream
				    :end (min 120 length))))
	    (declare (type (integer 0 #.array-dimension-limit) off end))
	    (loop while (octets= octets temp
			  :start1 off
			  :end1 (min (+ off 120) length)
			  :end2 end)
	      do
		(incf off end)
		(when (< off length)
		  (setf end (read-sequence temp stream
					   :end (min 120 (- length off))))))
	    (when (= off length)
	      (file-position stream (cdr entry))
	      (let* ((len (funcall (domain-entry-readfn domain) stream))
		     (off (funcall (domain-entry-readfn domain) stream))
		     (tmp (make-array len :element-type '(unsigned-byte 8))))
		(file-position stream off)
		(read-sequence tmp stream)
		(return (values tmp entry))))))))))

(defun domain-lookup (string domain)
  (declare (type string string) (type domain-entry domain)
	   (optimize (speed 3) (space 2)))
  (or (if (null (domain-entry-encoding domain)) string)
      (gethash string (domain-entry-hash domain))
      (let* ((octets (string-to-octets string
				       (domain-entry-encoding domain)))
	     (length (length octets))
	     (pos (gethash length (domain-entry-hash domain))))
	(declare (type (simple-array (unsigned-byte 8) (*)) octets))
	(multiple-value-bind (tmp entry) (search-domain octets domain pos)
	  (declare (type (or null (simple-array (unsigned-byte 8) (*))) tmp))
	  (when tmp
	    (let ((temp (delete entry pos :test #'eq)))
	      (if temp
		  (setf (gethash length (domain-entry-hash domain)) temp)
		  (remhash length (domain-entry-hash domain))))
	    (setf (gethash (copy-seq string) (domain-entry-hash domain))
		(octets-to-string tmp (domain-entry-encoding domain))))))))

(defun domain-lookup-plural (singular plural domain)
  (declare (type string singular plural) (type domain-entry domain)
	   (optimize (speed 3) (space 2)))
  (or (if (null (domain-entry-encoding domain)) nil)
      (gethash (cons singular plural) (domain-entry-hash domain))
      (let* ((octets (let* ((a (string-to-octets singular
					       (domain-entry-encoding domain)))
			    (b (string-to-octets plural
					       (domain-entry-encoding domain)))
			    (c (make-array (+ (length a) (length b) 1)
					   :element-type '(unsigned-byte 8))))
		       (declare (type (simple-array (unsigned-byte 8) (*))
				      a b c))
		       (replace c a)
		       (setf (aref c (length a)) 0)
		       (replace c b :start1 (+ (length a) 1))
		       c))
	     (length (length octets))
	     (pos (gethash length (domain-entry-hash domain))))
	(declare (type (simple-array (unsigned-byte 8) (*)) octets)
		 (type list pos))
	(multiple-value-bind (tmp entry) (search-domain octets domain pos)
	  (declare (type (or null (simple-array (unsigned-byte 8) (*))) tmp))
	  (when tmp
	    (prog1
		(setf (gethash (cons (copy-seq singular) (copy-seq plural))
			       (domain-entry-hash domain))
		    (loop for i = 0 then (1+ j)
			   as j = (position 0 tmp :start i)
		      collect (octets-to-string (subseq tmp i j)
						(domain-entry-encoding domain))
		      while j))
	      (let ((temp (delete entry pos :test #'eq)))
		(if temp
		    (setf (gethash length (domain-entry-hash domain)) temp)
		    (remhash length (domain-entry-hash domain))))
	      (when (null (domain-entry-plurals domain))
		(setf (domain-entry-plurals domain)
		    (parse-plurals domain)))))))))

(declaim (inline getenv)
	 (ftype (function (string) (or null string)) getenv))
(defun getenv (var)
  (let ((val #+CMU (cdr (assoc (intern var "KEYWORD") ext:*environment-list*))
	     #+scl (cdr (assoc var ext:*environment-list* :test 'string=))
	     #+SBCL (sb-ext:posix-getenv var)
	     #+Allegro (system:getenv var)
	     #+LispWorks (hcl:getenv var)
	     #+clisp (ext:getenv var)
	     #+(or openmcl mcl) (ccl::getenv var)
	     #+(or gcl ecl) (si::getenv var)))
    (if (equal val "") nil val)))

(defun setlocale (&optional locale)
  (setf *locale* (or locale
		     (getenv "LANGUAGE")
		     (getenv "LC_ALL")
		     (getenv "LC_MESSAGES")
		     (getenv "LANG")
		     *locale*)))

(defmacro textdomain (domain)
  `(eval-when #-gcl (:compile-toplevel :execute)
	      #+gcl (compile eval)
     (setf *default-domain* ,domain)))

(defmacro gettext (string)
  _N"Look up STRING in the current message domain and return its translation."
  `(dgettext ,*default-domain* ,string))

(defmacro ngettext (singular plural n)
  _N"Look up the singular or plural form of a message in the current domain."
  `(dngettext ,*default-domain* ,singular ,plural ,n))

(declaim (inline dgettext))
(defun dgettext (domain string)
  _N"Look up STRING in the specified message domain and return its translation."
  (declare (optimize (speed 3) (space 2)))
  (let ((domain (and domain (find-domain domain *locale*))))
    (or (and domain (domain-lookup string domain)) string)))

(defun dngettext (domain singular plural n)
  _N"Look up the singular or plural form of a message in the specified domain."
  (declare (type integer n)
	   (optimize (speed 3) (space 2)))
  (let* ((domain (and domain (find-domain domain *locale*)))
	 (list (and domain (domain-lookup-plural singular plural domain))))
    (if list
	(nth (the integer
	       (funcall (the function (domain-entry-plurals domain)) n))
	     list)
	(if (= n 1) singular plural))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-runtime
(defvar *translator-comment* nil)

#-runtime
(defvar *translations* (make-hash-table :test 'equal))

#-runtime
(defun note-translatable (domain string &optional plural)
  (when domain
    (let* ((hash (or (gethash domain *translations*)
		     (setf (gethash domain *translations*)
			   (make-hash-table :test 'equal))))
	   (key (if plural (cons string plural) string))
	   (val (or (gethash key hash) (cons nil nil))))
      (pushnew *translator-comment* (car val) :test #'equal)
      #-gcl
      (pushnew *compile-file-pathname* (cdr val) :test #'equal)
      (setf (gethash key hash) val)))
  (setq *translator-comment* nil))

;; GCL has define-compiler-macro, but it doesn't handle the case where
;; the form is returned.  Hence, disable these.  These were only
;; needed to note the translatable strings anyway, and maxima does
;; that in a different way.
#-gcl
(define-compiler-macro dgettext (&whole form domain string)
  #-runtime
  (when (and (stringp domain) (stringp string))
    (note-translatable domain string))
  form)

#-gcl
(define-compiler-macro dngettext (&whole form domain singular plural n)
  (declare (ignore n))
  #-runtime
  (when (and (stringp domain) (stringp singular) (stringp plural))
    (note-translatable domain singular plural))
  form)

(defun read-translatable-string (stream char)
  (declare (ignore char))
    (case (peek-char nil stream nil nil t)
      (#\" (let ((*read-suppress* nil)
		 (string (read stream t nil t)))
	     `(gettext ,string)))
      (#\N (read-char stream t nil t)
	   (let ((*read-suppress* nil)
		 (string (read stream t nil t)))
	     #-runtime
	     (note-translatable *default-domain* string)
	     string))
      (#\@ (error _"_@ is a reserved reader macro prefix."))
      (otherwise
       (let ((fn (get-macro-character #\_ nil)))
	 (if fn (funcall fn stream #\_) '_)))))

#-runtime
(defun read-comment (stream char)
  (declare (optimize (speed 0) (space 3) #-gcl (debug 0))
	   (ignore char))
  (do ((state 0)
       (index 0)
       (text nil)
       (char (read-char stream nil nil t) (read-char stream nil nil t)))
      ((or (not char) (char= char #\Newline))
       (when text (setq *translator-comment* (copy-seq text))))
    (cond ((and (= state 0) (char= char #\Space)) (setq state 1))
	  ((and (= state 0) (char= char #\T)) (setq state 1 index 1))
	  ((and (= state 0) (char/= char #\;)) (setq state 2))
	  ((and (= state 1) (= index 0) (char= char #\Space)) #|ignore|#)
	  ((= state 1)
	   (if (char= char (char "TRANSLATORS: " index))
	       (when (= (incf index) 13)
		 (setq state 3))
	       (setq state 2)))
	  ((= state 3)
	   (when (null text)
	     (setq text (make-array 50 :element-type 'character
				    :adjustable t :fill-pointer 0)))
	   (vector-push-extend char text))))
  (values))

#-runtime
(defun read-nested-comment (stream subchar arg)
  (declare (ignore subchar arg)
	   (optimize (speed 0) (space 3) #-gcl (debug 0)))
  (do ((level 1)
       (state 0)
       (index 0)
       (text nil)
       (prev (read-char stream t nil t) char)
       (char (read-char stream t nil t) (read-char stream t nil t)))
      (())
    (cond ((and (char= prev #\|) (char= char #\#))
	   (when (zerop (decf level))
	     (when text
	       (setq *translator-comment*
		     (string-right-trim '(#\Space #\Newline) text)))
	     (return)))
	  ((and (char= prev #\#) (char= char #\|))
	   (setq state 2)
	   (incf level))
	  ((and (= state 0) (char= prev #\Space)) (setq state 1))
	  ((and (= state 0) (char= prev #\T))
	   (setq state 1 index 1))
	  ((= state 0) (setq state 2))
	  ((and (= state 1) (= index 0) (char= prev #\Space)) #| ignore |#)
	  ((= state 1)
	   (if (char= prev (char "TRANSLATORS: " index))
	       (when (= (incf index) 13)
		 (setq state 3))
	       (setq state 2)))
	  ((= state 3)
	   (when (null text)
	     (setq text (make-array 50 :element-type 'character
				    :adjustable t :fill-pointer 0)))
	   (vector-push-extend prev text))))
  (values))

(defun install ()
  (set-macro-character #\_ #'read-translatable-string t)
  #-runtime
  (set-macro-character #\; #'read-comment)
  #-runtime
  (set-dispatch-macro-character #\# #\| #'read-nested-comment)
  t)


#-(or gcl runtime)
(defun dump-pot-files (&key copyright)
  (declare (optimize (speed 0) (space 3) #-gcl (debug 1)))
  (labels ((b (key data)
	     (format t "~@[~{~&#. ~A~}~%~]" (delete nil (car data)))
	     (format t "~@[~&~<#: ~@;~@{~A~^ ~}~:@>~%~]"
		     (delete nil (cdr data)))
	     (cond ((consp key)
		    (format t "~&msgid ") (str (car key) 6 0)
		    (format t "~&msgid_plural ") (str (cdr key) 13 0)
		    (format t "~&msgstr[0] \"\"~2%"))
		   (t
		    (format t "~&msgid ") (str key 6 0)
		    (format t "~&msgstr \"\"~2%"))))
	   (str (string col start)
	     (when (and (plusp col) (> (length string) (- 76 col)))
	       (format t "\"\"~%"))
	     (let ((nl (position #\Newline string :start start)))
	       (cond ((and nl (< (- nl start) 76))
		      (write-char #\")
		      (wstr string start nl)
		      (format t "\\n\"~%")
		      (str string 0 (1+ nl)))
		     ((< (- (length string) start) 76)
		      (write-char #\")
		      (wstr string start (length string))
		      (write-char #\"))
		     (t
		      (let* ((a (+ start 1))
			     (b (+ start 76))
			     (b1 (position #\Space string :start a :end b
					   :from-end t))
			     (b2 (position-if (lambda (x)
						(position x ";:,?!)]}"))
					      string :start a :end b
					      :from-end t))
			     (b3 (position-if (lambda (x)
						(position x "\"'-"))
					      string :start a :end b
					      :from-end t))
			     (b4 (position-if #'digit-char-p
					      string :start a :end b
					      :from-end t))
			     (b5 (position-if #'alpha-char-p
					      string :start a :end b
					      :from-end t))
			     (g1 (if b1 (* (- b b1) (- b b1) .03) 10000))
			     (g2 (if b2 (* (- b b2) (- b b2) .20) 10000))
			     (g3 (if b3 (* (- b b3) (- b b3) .97) 10000))
			     (g4 (if b4 (* (- b b4) (- b b4) 1.3) 10000))
			     (g5 (if b5 (* (- b b5) (- b b5) 2.0) 10000))
			     (g (min g1 g2 g3 g4 g5))
			     (end (1+ (cond ((> g 750) b)
					    ((= g g1) b1)
					    ((= g g2) b2)
					    ((= g g3) b3)
					    ((= g g4) b4)
					    ((= g g5) b5)))))
			#+(or)
			(progn
			  (format t "~&Splitting ~S:~%"
				  (subseq string start b))
			  (format t "~{~&  b~D=~D; goodness=~F~}~%"
				  (list 1 b1 g1 2 b2 g2 3 b3 g3 4 b4 g4 5 b5 g5
					6 b 10000))
			  (format t "~&  best=~F == ~D~%" g end)
			  (format t "~&  Part1=~S~%  Part2=~S~%"
				  (subseq string start end)
				  (subseq string end b)))
			(write-char #\")
			(wstr string start end)
			(write-char #\") (terpri)
			(str string 0 end))))))
	   (wstr (string start end)
	     (loop while (< start end) do
	       (let ((i (position-if (lambda (x)
				       (or (char= x #\") (char= x #\\)))
				     string :start start :end end)))
		 (write-string string nil :start start :end (or i end))
		 (when i (write-char #\\ nil) (write-char (char string i) nil))
		 (setq start (if i (1+ i) end)))))
	   (a (domain hash)
	     (format t "~&#@ ~A~2%" domain)
	     (format t "~&# SOME DESCRIPTIVE TITLE~%")
	     (format t "~@[~&# Copyright (C) YEAR ~A~%~]" copyright)
	     (format t "~&# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR~%")
	     (format t "~&#~%#, fuzzy~%msgid \"\"~%msgstr \"\"~%")
	     (format t "~&\"Project-Id-Version: PACKAGE VERSION\\n\"~%")
	     (format t "~&\"Report-Msgid-Bugs-To: \\n\"~%")
	     (format t "~&\"PO-Revision-Date: YEAR-MO-DA HO:MI +ZONE\\n\"~%")
	     (format t "~&\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"~%")
	     (format t "~&\"Language-Team: LANGUAGE <LL@li.org>\\n\"~%")
	     (format t "~&\"MIME-Version: 1.0\\n\"~%")
	     (format t "~&\"Content-Type: text/plain; charset=UTF-8\\n\"~%")
	     (format t "~&\"Content-Transfer-Encoding: 8bit\\n\"~2%")
	     (maphash #'b hash)))
    (maphash #'a *translations*)
    #+(or)
    (clrhash *translations*))
  nil)



(eval-when #-gcl (:compile-toplevel :execute)
	   #+gcl (compile eval)
  (setq *default-domain* "maxima")
  (unless (and (fboundp 'intl:read-translatable-string)
	       (eq (get-macro-character #\_)
		   (fdefinition 'intl:read-translatable-string)))
    (set-syntax-from-char #\_ #\_)))
