;;********************************************************
;; file:        init-cl.lisp
;; description: Initialize Maxima
;; date:        Wed Jan 13 1999 - 20:27
;; author:      Liam Healy <Liam.Healy@nrl.navy.mil>
;;********************************************************

;;; An ANSI-CL portable initializer to replace init_max1.lisp

;; CL-USER:*MAXIMA-BUILD-TIME* is defined in maxima.asd and maxima.system,
;; but I guess ECL doesn't see that, so define it here.
#+ecl (progn
  (in-package :cl-user)
  (defvar *maxima-build-time* '#.(multiple-value-list (get-decoded-time)))
  (export '*maxima-build-time*))

(in-package :maxima)

;;; Locations of various types of files. These variables are discussed
;;; in more detail in the file doc/implementation/dir_vars.txt. Since
;;; these are already in the maxima package, the maxima- prefix is
;;; redundant. It is kept for consistency with the same variables in
;;; shell scripts, batch scripts and environment variables.
;;; jfa 02/07/04

(defvar *maxima-prefix*)
(defvar *maxima-imagesdir*)
(defvar *maxima-sharedir*)
(defvar *maxima-srcdir*)
(defvar *maxima-docdir*)
(defvar *maxima-infodir*)
(defvar *maxima-htmldir*)
(defvar *maxima-layout-autotools*)
(defvar *maxima-userdir*)
(defvar *maxima-initmac* "maxima-init.mac")
(defvar *maxima-initlisp* "maxima-init.lisp")
(defvar *maxima-tempdir*)
(defvar *maxima-lang-subdir* nil)
(defvar *maxima-demodir*)
(defvar *maxima-objdir*)		;; Where to store object (fasl) files.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro def-lisp-shadow (root-name)
    "Create a maxima variable $root_name that is an alias for the lisp name *root-name*.
When one changes, the other does too."
    (let ((maxima-name (intern (concatenate 'string "$"
					    (substitute #\_ #\- (string root-name)))))
	  (lisp-name (intern (concatenate 'string "*" (string root-name) "*"))))
      `(progn
	 (defmvar ,maxima-name)
	 (putprop ',maxima-name 'shadow-string-assignment 'assign)
	 (putprop ',maxima-name ',lisp-name 'lisp-shadow)))))

(def-lisp-shadow maxima-tempdir)
(def-lisp-shadow maxima-userdir)
(def-lisp-shadow maxima-objdir)

(defun shadow-string-assignment (var value)
  (cond
    ((stringp value)
     (set (get var 'lisp-shadow) value)
     value)
    (t
      (merror (intl:gettext "assignment: must assign a string to ~:M; found: ~M") var value))))

(defun print-directories ()
  (format t "maxima-prefix=~a~%" *maxima-prefix*)
  (format t "maxima-imagesdir=~a~%" *maxima-imagesdir*)
  (format t "maxima-sharedir=~a~%" *maxima-sharedir*)
  (format t "maxima-srcdir=~a~%" *maxima-srcdir*)
  (format t "maxima-demodir=~a~%" *maxima-demodir*)
  (format t "maxima-testsdir=~a~%" *maxima-testsdir*)
  (format t "maxima-docdir=~a~%" *maxima-docdir*)
  (format t "maxima-infodir=~a~%" *maxima-infodir*)
  (format t "maxima-htmldir=~a~%" *maxima-htmldir*)
  (format t "maxima-plotdir=~a~%" *maxima-plotdir*)
  (format t "maxima-layout-autotools=~a~%" *maxima-layout-autotools*)
  (format t "maxima-userdir=~a~%" *maxima-userdir*)
  (format t "maxima-tempdir=~a~%" *maxima-tempdir*)
  (format t "maxima-lang-subdir=~a~%" *maxima-lang-subdir*)
  (format t "maxima-objdir=~A~%" *maxima-objdir*))

(defvar *maxima-lispname*
        #+clisp "clisp"
	#+cmu "cmucl"
	#+scl "scl"
	#+sbcl "sbcl"
	#+gcl "gcl"
	#+allegro "acl"
	#+openmcl "openmcl"
	#+abcl "abcl"
	#+lispworks "lispworks"
	#+ecl "ecl"
	#-(or clisp cmu scl sbcl gcl allegro openmcl abcl lispworks ecl) "unknownlisp")

(defvar $file_search_lisp nil
  "Directories to search for Lisp source code.")

(defvar $file_search_maxima nil
  "Directories to search for Maxima source code.")

(defvar $file_search_demo nil
  "Directories to search for demos.")

(defvar $file_search_usage nil)

(defvar $file_search_tests nil
  "Directories to search for maxima test suite")

(defun combine-path (&rest list)
  "splice a '/' between the path components given as arguments"
  (format nil "~{~A~^/~}" list))

(defun maxima-parse-dirstring (str)
  (let ((sep "/"))
    (if (position (character "\\") str)
	(setq sep "\\"))
    (setf str (concatenate 'string (string-right-trim sep str) sep))
    (concatenate 'string
		 (let ((dev (pathname-device str)))
		   (if (consp dev)
		       (setf dev (first dev)))
		   (if (and dev (not (eq dev :unspecific))
			    (not (string= dev "")))
		       (concatenate 'string (string-right-trim ":" dev) ":")
		       ""))
		 "/"
		 (apply #'combine-path (rest (pathname-directory str))))))

(defun set-pathnames-with-autoconf (maxima-prefix-env)
  (let (libdir libexecdir datadir infodir
	(package-version (combine-path *autoconf-package* *autoconf-version*))
	(binary-subdirectory (concatenate 'string "binary-" *maxima-lispname*)))
    (if maxima-prefix-env
	(progn
	  (setq libdir     (combine-path maxima-prefix-env "lib"))
	  (setq libexecdir (combine-path maxima-prefix-env "libexec"))
	  (setq datadir    (combine-path maxima-prefix-env "share"))
	  (setq infodir    (combine-path maxima-prefix-env "info")))
	(progn
	  (setq libdir     (maxima-parse-dirstring *autoconf-libdir*))
	  (setq libexecdir (maxima-parse-dirstring *autoconf-libexecdir*))
	  (setq datadir    (maxima-parse-dirstring *autoconf-datadir*))
	  (setq infodir    (maxima-parse-dirstring *autoconf-infodir*))))
    (setq *maxima-imagesdir* (combine-path libdir package-version binary-subdirectory))
    (setq *maxima-sharedir*  (combine-path datadir package-version "share"))
    (setq *maxima-srcdir*    (combine-path datadir package-version "src"))
    (setq *maxima-demodir*   (combine-path datadir package-version "demo"))
    (setq *maxima-testsdir*  (combine-path datadir package-version "tests"))
    (setq *maxima-docdir*    (combine-path datadir package-version "doc"))
    (setq *maxima-infodir*   infodir)
    (setq *maxima-htmldir*   (combine-path datadir package-version "doc" "html"))
    (setq *maxima-plotdir*   (combine-path libexecdir package-version))))

(defun set-pathnames-without-autoconf (maxima-prefix-env)
  (let ((maxima-prefix (if maxima-prefix-env
			   maxima-prefix-env
			   (maxima-parse-dirstring *autoconf-prefix*)))
	(binary-subdirectory (concatenate 'string "binary-" *maxima-lispname*)))

    (setq *maxima-imagesdir* (combine-path maxima-prefix "src" binary-subdirectory))
    (setq *maxima-sharedir*  (combine-path maxima-prefix "share"))
    (setq *maxima-srcdir*    (combine-path maxima-prefix "src"))
    (setq *maxima-demodir*   (combine-path maxima-prefix "demo"))
    (setq *maxima-testsdir*  (combine-path maxima-prefix "tests"))
    (setq *maxima-docdir*    (combine-path maxima-prefix "doc"))
    (setq *maxima-infodir*   (combine-path maxima-prefix "doc" "info"))
    (setq *maxima-htmldir*   (combine-path maxima-prefix "doc" "html"))
    (setq *maxima-plotdir*   (combine-path maxima-prefix "plotting"))))

(defun default-userdir ()
  (let ((home-env (maxima-getenv "HOME"))
	(base-dir "")
	(maxima-dir (if (string= *autoconf-win32* "true")
			"maxima"
			".maxima")))
    (setf base-dir
	  (if (and home-env (string/= home-env ""))
	      ;; use home-env...
	      (if (string= home-env "c:\\")
		  ;; but not if home-env = c:\, which results in slow startups
		  ;; under windows. Ick.
		  "c:\\user\\"
		  home-env)
	      ;; we have to make a guess
	      (if (string= *autoconf-win32* "true")
		  "c:\\user\\"
		  "/tmp")))
    (combine-path (maxima-parse-dirstring base-dir) maxima-dir)))

(defun default-tempdir ()
  (let ((home-env (maxima-getenv "HOME"))
	(base-dir ""))
    (setf base-dir
	  (if (and home-env (string/= home-env ""))
	      (if (string= home-env "c:\\")
		  "c:\\user\\"
		  home-env)
	      (if (string= *autoconf-win32* "true")
		  "c:\\user\\"
		  "/tmp")))
    (maxima-parse-dirstring base-dir)))

(defun set-locale-subdir ()
  (let (language territory codeset)
    ;; Determine *maxima-lang-subdir*
    ;;   1. from MAXIMA_LANG_SUBDIR environment variable
    ;;   2. from INTL::*LOCALE* if (1) fails
    (unless  (setq *maxima-lang-subdir* (maxima-getenv "MAXIMA_LANG_SUBDIR"))
      (cond ((or (null intl::*locale*) (equal intl::*locale* ""))
	     (setq *maxima-lang-subdir* nil))
	      ((member intl::*locale* '("C" "POSIX" "c" "posix") :test #'equal)
	       (setq *maxima-lang-subdir* nil))
	      (t  (when (eql (position #\. intl::*locale*) 5)
		    (setq codeset (string-downcase (subseq intl::*locale* 6))))
		  (when (eql (position #\_ intl::*locale*) 2)
		    (setq territory (string-downcase (subseq intl::*locale* 3 5))))
		  (setq language (string-downcase (subseq intl::*locale* 0 2)))
		  ;; Set *maxima-lang-subdir* only for known languages.
		  ;; Extend procedure below as soon as new translation
		  ;; is available.
		  (cond ((equal language "en") ;; English
			 (setq *maxima-lang-subdir* nil))
			;; Latin-1 aka iso-8859-1 languages
			((member language '("es" "pt" "fr" "de" "it") :test #'equal)
			 (if (and (string= language "pt") (string= territory "br"))
			     (setq *maxima-lang-subdir* (concatenate 'string language "_BR"))
			     (setq *maxima-lang-subdir* language))
			 (if (member codeset '("utf-8" "utf8") :test #'equal)
			     (setq *maxima-lang-subdir* (concatenate 'string *maxima-lang-subdir* ".utf8"))))
			;; Russian. Default codepage cp1251
			((string= language "ru")
			 (setq *maxima-lang-subdir* language)
			 (cond ((member codeset '("utf-8" "utf8") :test #'equal)
				(setq *maxima-lang-subdir* (concatenate 'string *maxima-lang-subdir* ".utf8")))
			       ((member codeset '("koi8-r" "koi8r") :test #'equal)
				(setq *maxima-lang-subdir* (concatenate 'string *maxima-lang-subdir* ".koi8r")))))
			(t  (setq *maxima-lang-subdir* nil))))))))

(defun set-pathnames ()
  (let ((maxima-prefix-env (maxima-getenv "MAXIMA_PREFIX"))
	(maxima-layout-autotools-env (maxima-getenv "MAXIMA_LAYOUT_AUTOTOOLS"))
	(maxima-userdir-env (maxima-getenv "MAXIMA_USERDIR"))
	(maxima-tempdir-env (maxima-getenv "MAXIMA_TEMPDIR"))
	(maxima-objdir-env (maxima-getenv "MAXIMA_OBJDIR")))
    ;; MAXIMA_DIRECTORY is a deprecated substitute for MAXIMA_PREFIX
    (unless maxima-prefix-env
      (setq maxima-prefix-env (maxima-getenv "MAXIMA_DIRECTORY")))
    (if maxima-prefix-env
	(setq *maxima-prefix* maxima-prefix-env)
	(setq *maxima-prefix* (maxima-parse-dirstring *autoconf-prefix*)))
    (if maxima-layout-autotools-env
	(setq *maxima-layout-autotools*
	      (string-equal maxima-layout-autotools-env "true"))
	(setq *maxima-layout-autotools*
	      (string-equal *maxima-default-layout-autotools* "true")))
    (if *maxima-layout-autotools*
	(set-pathnames-with-autoconf maxima-prefix-env)
	(set-pathnames-without-autoconf maxima-prefix-env))
    (if maxima-userdir-env
	(setq *maxima-userdir* (maxima-parse-dirstring maxima-userdir-env))
	(setq *maxima-userdir* (default-userdir)))
    (if maxima-tempdir-env
	(setq *maxima-tempdir* (maxima-parse-dirstring maxima-tempdir-env))
	(setq *maxima-tempdir* (default-tempdir)))
    ;; Default *MAXIMA-OBJDIR* is <userdir>/binary/binary-<foo>lisp,
    ;; because userdir is almost surely writable, and we don't want to clutter up
    ;; random directories with Maxima stuff.
    ;; Append binary-<foo>lisp whether objdir is the default or obtained from environment.
    (setq *maxima-objdir*
          (concatenate 'string
                       (if maxima-objdir-env
                         (maxima-parse-dirstring maxima-objdir-env)
                         (concatenate 'string *maxima-userdir* "/binary"))
                       "/binary-" *maxima-lispname*))

    ; On Windows Vista gcc requires explicit include
    #+gcl
    (when (string= *autoconf-win32* "true")
      (let ((mingw-gccver (maxima-getenv "mingw_gccver")))
	(when mingw-gccver
	  (setq compiler::*cc*
		(concatenate 'string compiler::*cc* " -I\"" *maxima-prefix* "\\include\""
			     " -I\"" *maxima-prefix* "\\lib\\gcc-lib\\mingw32\\"
			     mingw-gccver "\\include\" ")))))

    ; Assign initial values for Maxima shadow variables
    (setq $maxima_userdir *maxima-userdir*)
    (setf (gethash '$maxima_userdir *variable-initial-values*) *maxima-userdir*)
    (setq $maxima_tempdir *maxima-tempdir*)
    (setf (gethash '$maxima_tempdir *variable-initial-values*) *maxima-tempdir*)
    (setq $maxima_objdir *maxima-objdir*)
    (setf (gethash '$maxima_objdir *variable-initial-values*) *maxima-objdir*))

  (let* ((ext #+gcl "o"
	      #+(or cmu scl) (c::backend-fasl-file-type c::*target-backend*)
	      #+sbcl "fasl"
	      #+clisp "fas"
	      #+allegro "fasl"
	      #+openmcl (pathname-type ccl::*.fasl-pathname*)
	      #+lispworks (pathname-type (compile-file-pathname "foo.lisp"))
	      #+ecl "fas"
	      #-(or gcl cmu scl sbcl clisp allegro openmcl lispworks ecl)
	      "")
	 (lisp-patterns (concatenate 'string "$$$.{" ext ",lisp,lsp}"))
	 (maxima-patterns "$$$.{mac,mc}")
	 (lisp+maxima-patterns (concatenate 'string "$$$.{" ext ",lisp,lsp,mac,mc}"))
	 (demo-patterns "$$$.{dem,dm1,dm2,dm3,dmt}")
	 (usage-patterns "$$.{usg,texi}")
	 (share-subdirs-list (share-subdirs-list))
	 ;; Smash the list of share subdirs into a string of the form
	 ;; "{affine,algebra,...,vector}" .
	 (share-subdirs (format nil "{~{~A~^,~}}" share-subdirs-list)))

    (setq $file_search_lisp
	  (list '(mlist)
		;; actually, this entry is not correct.
		;; there should be a separate directory for compiled
		;; lisp code. jfa 04/11/02
		(combine-path *maxima-userdir* lisp-patterns)
		(combine-path *maxima-sharedir* lisp-patterns)
		(combine-path *maxima-sharedir* share-subdirs lisp-patterns)
		(combine-path *maxima-srcdir* lisp-patterns)))
    (setq $file_search_maxima
	  (list '(mlist)
		(combine-path *maxima-userdir* maxima-patterns)
		(combine-path *maxima-sharedir* maxima-patterns)
		(combine-path *maxima-sharedir* share-subdirs maxima-patterns)))
    (setq $file_search_demo
	  (list '(mlist)
		(combine-path *maxima-sharedir* demo-patterns)
		(combine-path *maxima-sharedir* share-subdirs demo-patterns)
		(combine-path *maxima-demodir* demo-patterns)))
    (setq $file_search_usage
	  (list '(mlist)
		(combine-path *maxima-sharedir* usage-patterns)
		(combine-path *maxima-sharedir* share-subdirs usage-patterns)
		(combine-path *maxima-docdir* usage-patterns)))
    (setq $file_search_tests
	  `((mlist) ,(combine-path *maxima-testsdir* lisp+maxima-patterns)))

    ;; If *maxima-lang-subdir* is not nil test whether corresponding info directory
    ;; with some data really exists.  If not this probably means that required
    ;; language pack wasn't installed and we reset *maxima-lang-subdir* to nil.
    (when (and *maxima-lang-subdir*
	       (not (probe-file (combine-path *maxima-infodir* *maxima-lang-subdir* "maxima-index.lisp"))))
       (setq *maxima-lang-subdir* nil))))

(defun get-dirs (path)
  #+(or :clisp :sbcl :ecl :openmcl)
  (directory (concatenate 'string (namestring path) "/*/")
	     #+openmcl :directories #+openmcl t)
  #-(or :clisp :sbcl :ecl :openmcl)
  (directory (concatenate 'string (namestring path) "/*")))

(defun unix-like-basename (path)
  (let* ((pathstring (namestring path))
	 (len (length pathstring)))
    (when (equal (subseq pathstring (- len 1) len) "/")
      (decf len)
      (setf pathstring (subseq pathstring 0 len)))
    (subseq pathstring (1+ (or (position #\/ pathstring :from-end t)
			       (position #\\ pathstring :from-end t))) len)))

(defun unix-like-dirname (path)
  (let* ((pathstring (namestring path))
	 (len (length pathstring)))
    (when (equal (subseq pathstring (- len 1) len) "/")
      (decf len)
      (setf pathstring (subseq pathstring 0 len)))
    (subseq pathstring 0 (or (position #\/ pathstring :from-end t)
			     (position #\\ pathstring :from-end t)))))

(defun list-avail-action ()
  (let* ((maxima-verpkglibdir (if (maxima-getenv "MAXIMA-VERPKGLIBDIR")
				  (maxima-getenv "MAXIMA-VERPKGLIBDIR")
				  (if (maxima-getenv "MAXIMA_PREFIX")
				      (combine-path (maxima-getenv "MAXIMA_PREFIX") "lib"
						    *autoconf-package* *autoconf-version*)
				      (combine-path (maxima-parse-dirstring *autoconf-libdir*)
						    *autoconf-package* *autoconf-version*))))
	 (len (length maxima-verpkglibdir))
	 (lisp-string nil))
    (format t "Available versions:~%")
    (unless (equal (subseq maxima-verpkglibdir (- len 1) len) "/")
      (setf maxima-verpkglibdir (concatenate 'string maxima-verpkglibdir "/")))
    (dolist (version (get-dirs (unix-like-dirname maxima-verpkglibdir)))
      (dolist (lisp (get-dirs version))
	(setf lisp-string (unix-like-basename lisp))
	(when (search "binary-" lisp-string)
	  (setf lisp-string (subseq lisp-string (length "binary-") (length lisp-string)))
	  (format t "version ~a, lisp ~a~%" (unix-like-basename version) lisp-string))))
    (bye)))

(defun process-maxima-args (input-stream batch-flag)
  ;;    (format t "processing maxima args = ")
  ;;    (mapc #'(lambda (x) (format t "\"~a\"~%" x)) (get-application-args))
  ;;    (terpri)
  (let ((maxima-options nil))
    ;; Note: The current option parsing code expects every short
    ;; option to have an equivalent long option.  No check is made for
    ;; this, so please make sure this holds.  Or change the code in
    ;; process-args in command-line.lisp.
    (setf maxima-options
	  (list
	   (make-cl-option :names '("-b" "--batch")
			   :argument "<file>"
			   :action #'(lambda (file)
				       (setf input-stream
					     (make-string-input-stream
					      (format nil "batch(\"~a\");"
						      file)))
				       (setf batch-flag :batch))
			   :help-string
			   "Process maxima file <file> in batch mode.")
	   (make-cl-option :names '("--batch-lisp")
			   :argument "<file>"
			   :action #'(lambda (file)
				       (setf input-stream
					     (make-string-input-stream
					      #-sbcl (format nil ":lisp (load \"~a\");" file)
					      #+sbcl (format nil ":lisp (with-compilation-unit nil (load \"~a\"));" file)))
				       (setf batch-flag :batch))
			   :help-string
			   "Process lisp file <file> in batch mode.")
	   (make-cl-option :names '("--batch-string")
			   :argument "<string>"
			   :action #'(lambda (string)
				       (setf input-stream
					     (make-string-input-stream string))
				       (setf batch-flag :batch))
			   :help-string
			   "Process maxima command(s) <string> in batch mode.")
	   (make-cl-option :names '("-d" "--directories")
			   :action #'(lambda () (print-directories) ($quit))
			   :help-string
			   "Display maxima internal directory information.")
	   (make-cl-option :names '("--disable-readline")
			   :action #'(lambda ()
				       #+gcl
				       (if (find :readline *features*)
					   (si::readline-off)))
			   :help-string "Disable readline support.")
	   (make-cl-option :names '("-g" "--enable-lisp-debugger")
			   :action #'(lambda ()
				       (setf *debugger-hook* nil))
			   :help-string
			   "Enable underlying lisp debugger.")
	   (make-cl-option :names '("-h" "--help")
			   :action #'(lambda ()
				       (format t "usage: maxima [options]~%")
				       (list-cl-options maxima-options)
				       (bye))
			   :help-string "Display this usage message.")
	   (make-cl-option :names '("--userdir")
			   :argument "<directory>"
			   :action nil
			   :help-string "Use  <directory> for user directory (default is $HOME/maxima for Windows, and $HOME/.maxima for others)")
 	   (make-cl-option :names '("--init")
			   :argument "<file>"
			   :action #'(lambda (file)
				       (setf *maxima-initmac* (concatenate 'string file ".mac"))
				       (setf *maxima-initlisp* (concatenate 'string file ".lisp")))
			   :help-string (format nil "Set the name of the Maxima & Lisp initialization files to <file>.mac & <file>.lisp (default is ~a)" (subseq *maxima-initmac* 0 (- (length *maxima-initmac*) 4))))
 	   (make-cl-option :names '("--init-mac")
			   :argument "<file>"
			   :action #'(lambda (file)
				       (setf *maxima-initmac* file))
			   :help-string (format nil "Set the name of the Maxima initialization file (default is ~a)" *maxima-initmac*))
 	   (make-cl-option :names '("--init-lisp")
			   :argument "<file>"
			   :action #'(lambda (file)
				       (setf *maxima-initlisp* file))
			   :help-string (format nil "Set the name of the Lisp initialization file (default is ~a)" *maxima-initlisp*))
	   (make-cl-option :names '("-l" "--lisp")
			   :argument "<lisp>"
			   :action nil
			   :help-string "Use lisp implementation <lisp>.")
	   (make-cl-option :names '("--list-avail")
			   :action 'list-avail-action
			   :help-string
			   "List the installed version/lisp combinations.")
	   (make-cl-option :names '("-p" "--preload-lisp")
			   :argument "<lisp-file>"
			   :action #'(lambda (file)
				       #-sbcl (load file) #+sbcl (with-compilation-unit nil (load file)))
			   :help-string "Preload <lisp-file>.")
	   (make-cl-option :names '("-q" "--quiet")
			   :action #'(lambda () (declare (special *maxima-quiet*)) (setq *maxima-quiet* t))
			   :help-string "Suppress Maxima start-up message.")
	   (make-cl-option :names '("-r" "--run-string")
			   :argument "<string>"
			   :action #'(lambda (string)
				       (setf input-stream
					     (make-string-input-stream string))
				       (setf batch-flag nil))
			   :help-string
			   "Process maxima command(s) <string> in interactive mode.")
	   (make-cl-option :names '("-s" "--server")
			   :argument "<port>"
			   :action #'(lambda (port-string)
				       (start-client (parse-integer
						      port-string)))
			   :help-string "Connect Maxima to server on <port>.")
	   (make-cl-option :names '("-u" "--use-version")
			   :argument "<version>"
			   :action nil
			   :help-string "Use maxima version <version>.")
	   (make-cl-option :names '("-v" "--verbose")
			   :action nil
			   :help-string
			   "Display lisp invocation in maxima wrapper script.")
	   (make-cl-option :names '("--version")
			   :action #'(lambda ()
				       (format t "Maxima ~a~%"
					       *autoconf-version*)
				       ($quit))
			   :help-string
			   "Display the default installed version.")
	   (make-cl-option :names '("--very-quiet")
			   :action #'(lambda () (declare (special *maxima-quiet* *display-labels-p*))
					     (setq *maxima-quiet* t *display-labels-p* nil))
			   :help-string "Suppress expression labels and Maxima start-up message.")
	   (make-cl-option :names '("-X" "--lisp-options")
			   :argument "<Lisp options>"
			   :action #'(lambda (&rest opts)
				       (format t "Lisp options: ~A" opts))
			   :help-string "Options to be given to the underlying Lisp")
			   ))
    (process-args (get-application-args) maxima-options))
  (values input-stream batch-flag))

(defun cl-user::run ()
  "Run Maxima in its own package."
  (in-package :maxima)
  (initialize-runtime-globals)
  (let ((input-stream *standard-input*)
	(batch-flag nil))
    (catch 'to-lisp
      (setf (values input-stream batch-flag)
	    (process-maxima-args input-stream batch-flag))
      (loop
	 (with-simple-restart (macsyma-quit "Maxima top-level")
	   (macsyma-top-level input-stream batch-flag))))))

(defun initialize-runtime-globals ()
  (setf *load-verbose* nil)
  (setf *debugger-hook* #'maxima-lisp-debugger)
  ;; See discussion on the maxima list
  ;; http://www.math.utexas.edu/pipermail/maxima/2011/024014.html.
  ;; Set *print-length* and *print-level* to some reasonable values so
  ;; that normal Lisp structure is shown, but prevent typical circular
  ;; structures from hanging Lisp.
  ;;
  ;; (We do we set these instead of binding them?)
  (setf *print-circle* nil)
  (setf *print-length* 100)
  (setf *print-level* 15)
  
  ;; GCL: print special floats, which are generated whether or not this flag is enabled
  #+gcl (setf si:*print-nans* t)
  #+ccl
  (progn
    (setf ccl::*invoke-debugger-hook-on-interrupt* t)
    ;; CCL 1.5 makes *read-default-float-format* a thread-local
    ;; variable.  Hence we need to set it here to get our desired
    ;; behavior.
    (setf *read-default-float-format* 'double-float))

  #+allegro
  (progn
    (set-readtable-for-macsyma)
    (setf *read-default-float-format* 'lisp::double-float))

  (initialize-real-and-run-time)
  (intl::setlocale)
  (set-locale-subdir)
  (adjust-character-encoding)
  (set-pathnames)
  (cl-info::load-primary-index)   
  (when (boundp '*maxima-prefix*)
    (push (pathname (concatenate 'string *maxima-prefix*
                                 (if *maxima-layout-autotools*
                                     "/share/locale/"
                                     "/locale/")))
          intl::*locale-directories*)))

(defun adjust-character-encoding ()
  (ignore-errors
    #+clisp (progn (setf custom:*default-file-encoding*
                         (ext:make-encoding :input-error-action #\?))
                   (setf custom:*terminal-encoding*
                         custom:*default-file-encoding*))))

(import 'cl-user::run)

(defun $to_lisp ()
  (format t "~&Type (to-maxima) to restart, ($quit) to quit Maxima.~%")
  (let ((old-debugger-hook *debugger-hook*))
    (catch 'to-maxima
      (unwind-protect
	   (maxima-read-eval-print-loop)
	(setf *debugger-hook* old-debugger-hook)
	(format t "Returning to Maxima~%")))))

(defun to-maxima ()
  (throw 'to-maxima t))

(defun maxima-read-eval-print-loop ()
  (setf *debugger-hook* #'maxima-lisp-debugger-repl)
  (loop
     (catch 'to-maxima-repl
       (format t "~a~%~a> ~a" *prompt-prefix* (package-name *package*) *prompt-suffix*)
       (finish-output)
       (format t "~{~&~S~}" (multiple-value-list (eval (read)))))))

(defun maxima-lisp-debugger-repl (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (format t "~&Maxima encountered a Lisp error:~%~% ~A" condition)
  (format t "~&~%Automatically continuing.~%To reenable the Lisp debugger set *debugger-hook* to nil.~%")
  (throw 'to-maxima-repl t))

(defvar $help "type `describe(topic);' or `example(topic);' or `? topic'")

(defun $help (&rest dummy)
  (declare (ignore dummy))
  $help)

(eval-when (:load-toplevel :execute)
    (let ((context '$global))
      (declare (special context))
      (dolist (x '($%pi $%i $%e $%phi %i $%gamma  ;numeric constants
                   $inf $minf $und $ind $infinity ;pseudo-constants
                   t nil))                        ;logical constants (Maxima names: true, false)
	(kind x '$constant)
	(setf (get x 'sysconst) t))))

;;; Now that all of maxima has been loaded, define the various lists
;;; and hashtables of builtin symbols and values.

;;; The assume database structures for numeric constants such as $%pi and $%e
;;; are circular.  Attempting to copy a circular structure
;;; into *builtin-symbol-props* would cause a hang.  Therefore
;;; the properties are copied into *builtin-symbol-props* before
;;; initializing the assume database.
(let ((maxima-package (find-package :maxima)))
  (do-symbols (s maxima-package)
    (when (and (eql (symbol-package s) maxima-package)
	       (not (eq s '||))
	       (member (char (symbol-name s) 0) '(#\$ #\%) :test #'char=))
      (push s *builtin-symbols*)
      (setf (gethash s *builtin-symbol-props*)
	    (copy-tree (symbol-plist s))))))

;; Initialize assume database for $%pi, $%e, etc
(dolist (c *builtin-numeric-constants*)
  (initialize-numeric-constant c))

(dolist (s *builtin-symbols*)
  (when (boundp s)
    (push s *builtin-symbols-with-values*)))

(dolist (s *builtin-symbols-with-values*)
  (setf (gethash s *builtin-symbol-values*) (symbol-value s)))

(setf *builtin-$props* (copy-list $props))
(setf *builtin-$rules* (copy-list $rules))

(defun maxima-objdir (&rest subdirs)
  "Return a pathname string such that subdirs is a subdirectory of maxima_objdir"
  (apply #'combine-path *maxima-objdir* subdirs))

;; The directory part of *load-pathname*.  GCL doesn't seem to have a
;; usable *LOAD-PATHNAME*, but it does have SYS:*LOAD-PATHNAME*.
(defun maxima-load-pathname-directory ()
  "Return the directory part of *load-pathname*."
  (make-pathname :directory (pathname-directory #-gcl *load-pathname*
						#+gcl sys:*load-pathname*)))
