;;; -*-  Mode: Lisp; Package: MAKE; Syntax: Common-Lisp; Base: 10 -*- ;;;;
(in-package :make :use '(:common-lisp))

(export '(make system-load system-compile))
(provide "MAKE")
;;;  *******  Description of Make Facility ************
;;  We provide a simple MAKE facility to allow
;;compiling and loading of a tree of files
;;If the tree is '(a b (d e g h) i)
;;   a will be loaded before b is compiled,
;;   b will be loaded before d, e, g, h are compiled
;;   d e g h will be loaded before i is compiled.

;;  A record is kept of write dates of loaded compiled files, and a file
;;won't be reloaded if it is the same version (unless a force flag is t).

;;Thus if you do (make :uinfor) twice in a row, the second one would not
;;load anything.  NOTE: If you change a, and a macro in it would affect
;;b, b still will not be recompiled.  You must choose the :recompile t
;;option, to force the recompiling if you change macro files.
;;Alternately you may specify dependency information (see :depends below).


;;****** Sample file which when loaded causes system ALGEBRA
;;              to be compiled and loaded ******

;;(require "MAKE")
;;(use-package "MAKE")
;;(setf (get :algebra :make) '(a b (d e) l))
;;(setf (get :algebra :source-path) "/usr2/wfs/algebra/foo.lisp")
;;(setf (get :algebra :object-path) "/usr2/wfs/algebra/o/foo.o")
;;(make :algebra :compile t)

;;  More complex systems may need to do some special operations
;;at certain points of the make.
;;the tree of files may contain some keywords which have special meaning.
;;eg. '(a b (:progn (gbc) (if make::*compile*
;;                                  (format t "A and B finally compiled")))
;;          (:load-source h i)
;;          (d e) l)

;;then during the load and compile phases the function (gbc) will be
;;called after a and b have been acted on, and during the compile phase
;;the message about "A and B finally.." will be printed.
;;the lisp files h and i will be loaded after merging the paths with
;;the source directory.  This feature is extensible: see the definitions
;;of :load-source and :progn.

;;  The keyword feature is extensible, and you may specify what
;;happens during the load or compile phase for your favorite keyword.
;;To do this look at the definition of :progn, and :load-source
;;in the source for make.


;;Dependency feature:

;;   This make NEVER loads or compiles files in an order different from
;;that specified by the tree.  It will omit loading files which are
;;loaded and up to date, but if two files are out of date, the first (in
;;the printed representation of the tree), will always be loaded before
;;the second.  A consequence of this is that circular dependencies can
;;never occur.
;;
;;  If the :make tree contains (a b c d (:depends (c d) (a b))) then c
;;and d depend on a and b, so that if a or b need recompilation then c
;;and d will also be recompiled.  Thus the general form of a :depends
;;clause is (:depends later earlier) where LATER and EARLIER are either
;;a single file or a list of files. Read it as LATER depends on EARLIER.
;;A declaration of a (:depends (c) (d)) would have no effect, since the
;;order in the tree already rules out such a dependence.

;;  An easy way of specifying a linear dependence is by using :serial.
;;The tree (a (:serial b c d) e)  is completely equivalent to the tree
;;(a b c d e (:depends c b)(:depends d (b c))), but with a long list of
;;serial files, it is inconvenient to specify them in the
;;latter representation.

;;A common case is a set of macros whose dependence is serial followed by a set
;;of files whose order is unimportant.  A conventient way of building that
;;tree is
;;
;;(let ((macros '(a b c d))
;;      (files '(c d e f g)))
;;  `((:serial ,@ macros)
;;    ,files
;;    (:depends ,files ,macros)))

;;  The depends clause may occur anywhere within the tree, since
;;an initial pass collects all dependency information.

;;  Make takes a SHOW keyword argument.  It is almost impossible to simulate
;;all the possible features of make, for show.  Nonetheless, it is good
;;to get an idea of the compiling and loading sequence for a new system.
;;As a byproduct, you could use the output, as a simple sequence of calls
;;to compile-file and load, to do the required work, when make is not around
;;to help.


;;*****  Definitions ********
(defvar *files-loaded* nil)
(defvar *show-files-loaded* nil) ;only for show option
(defvar *load* nil "Will be non nil inside load-files")
(defvar *compile* nil "Bound by compile-files to t")
(defvar *depends* nil)
(defvar *depends-new* nil)
(defvar *force* nil)
(defvar *when-compile* nil "Each compile-file evals things in this list and sets it to nil")
#+kcl(defvar *system-p* nil)
(defvar *compile-file-function* 'make-compile-file)
(defvar *load-function* 'make-load-file)
(defvar show nil)
(defvar *cflags* #-kcl nil
  #+kcl '(:system-p  *system-p*))


;;this is the main entry point

(defun make (system &key recompile compile batch object-path source-path
	     show
	     &aux files *depends* *when-compile*
	     *show-files-loaded*)

  "SYSTEM is a tree of files, or a symbol with :make property.  It
loads all file files in system.  If COMPILE it will try to compile
files with newer source versions than object versions, before loading.
If RECOMPILE it will recompile all files.  This is equivalent to deleting all
objects and using :compile t.   SOURCE-PATH is merged with the name given
in the files list, when looking for a file to compile.  OBJECT-PATH is
merged with the name in the files list, when looking for a file to
load.  If SYSTEM is a symbol, then a null OBJECT-PATH would be set to
the :object-path property of SYSTEM.  Similarly for :source-path"

  (declare (special object-path source-path show)) batch
  (cond ((symbolp system)
	 (or object-path (setf object-path (get system :object-path)))
	 (or source-path (setf source-path (get system :source-path)))
	 (setf files (get system :make))
	 (or files
	     (if (get system :files)
		 (error "Use :make property, :files property is obssolet{!")))
	 )
	(t (setf files system)))
  (let ((*depends*  (if (or compile recompile) (get-depends system)))
	*depends-new*)
    (dolist (v files)
      (when (or compile recompile)
	(compile-files v recompile))
      (load-files v recompile))))

(defun system-load (system-name &rest names)
  "If :infor is a system, (system-load :uinfor joe betty) will load
joe and betty from the object-path for :uinfor"
  (load-files names t (get system-name :object-path)))

(defun system-compile (system-name &rest names)

  "If :iunfor is a system, (system-compile :uinfor joe) will in the
source path for joe and compile him into the object path for :uinfor"
  (compile-files names t :source-path
		 (get system-name :source-path) :object-path
		 (get system-name :object-path)))

(defun get-depends (system-name &aux result)
  (dolist (v (get system-name :make))
  (cond    ((atom v) )
	   ((eq (car v) :serial)
	    (do ((w (reverse (cdr v))(cdr w)))
		((null (cdr w)))
		(push (list (car w) (cdr w)) result)))
	   ((eq (car v) :depends)
	    (push (cdr v) result ))))
    result)

#+kcl
(setq si::*default-time-zone* 6)

(defun print-date (&optional(stream *standard-output*)
			    (time (get-universal-time)))
  (multiple-value-bind (sec min hr day mon yr wkday)
		       (decode-universal-time time)
	(format stream "~a ~a ~a ~d:~2,'0d:~2,'0d ~a"
		(nth wkday '( "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
		(nth (1- mon) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
			   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
		day
		hr min sec yr)))

;;This is an awfully roundabout downcase, but some machines
;;like symbolics swap cases on the pathname, so we have to do an extra
;;swap!!
(defun lowcase (na &aux (*print-case* :downcase))
  (pathname-name (pathname  (format nil "~a" na))))

(defun our-merge (name path &optional ign  ) ign
    (make-pathname :name (string name)
		   :type (pathname-type path)
		   :version (pathname-version path)
		   :host (pathname-host path)
		   :directory (pathname-directory path)))


#+kcl
(setf (get :link 'load)
      #'(lambda (path to-link)
	  (declare (special object-path))
	  (si::faslink (our-merge	(lowcase  path) object-path)
		       to-link)))

(setf (get :link 'compile)
      #'(lambda (path to-link)
	   to-link
	  (compile-files  path *force*)))

(setf (get :progn 'load)
      #'(lambda (&rest args)
	  (eval (cons 'progn args))))

(setf (get :progn 'compile) (get :progn 'load))

(setf (get :load-source 'load)
      #'(lambda (&rest args)
	  (declare (special source-path))
	  (load-files args *force* source-path)))

(setf (get :load-source-when-compile 'compile)
      (get :load-source 'load))

;;should nott use :lisp anymore
(setf (get :lisp 'load)
      #'(lambda (x) (error "please replace :lisp by :load-source")))

(setf (get :serial 'load) #'(lambda (&rest l)(load-files l)))
(setf (get :serial 'compile)
      #'(lambda (&rest l)
	  (dolist (v l)
	    (compile-files v)
	    (load-files v))))


(defun load-files (files &optional (*force* *force*) (object-path object-path)
			 &aux path tem (*load* t))
  (declare (special object-path source-path *force* show))
  (cond ((atom files)
	 (setq path (object files))
	 (cond (show
		(unless (member path *show-files-loaded* :test 'equalp)
			(push path *show-files-loaded*)
			(format t "~%(LOAD ~s)" (namestring path))))
	       ((null *load-function*))
	       ((or *force*
		    (or (not (setq tem
				   (member path *files-loaded*
					   :test 'equalp :key 'car)))
			(> (file-write-date  path) (cdr (car tem)))))
		(funcall *load-function* files)
		(push (cons path (file-write-date path)) *files-loaded*))))
	((keywordp (car files))
	 (let ((fun (get (car files) 'load)))
	   (cond (fun (apply fun (cdr files))))))
	(t (dolist (v files) (load-files v *force*  object-path)))))


(defun file-date (file)
  (if (probe-file file) (or (file-write-date file) 0) 0))

(defun source (file)
  (declare (special source-path))
   (our-merge  (lowcase file) source-path))

(defun object (file)
  (declare (special object-path))
   (our-merge  (lowcase file) object-path))


;;for lisp machines, and others where checking date is slow, this
;;we should try to cache some dates, and then remove them as we do
;;things like compile files...

(defun file-out-dated (file)
  (let ((obj-date (file-date (object file))))
    (or (<= obj-date (file-date (source file)))
	(dolist (v *depends*)
		(cond ((or (and (consp (car v))
				(member file (car v)))
			   (eq (car v) file))
		       (dolist (w (if (consp (second v))
				      (second v) (cdr v)))
			       (cond ((or (<= obj-date (file-date (source w)))
					  (member w *depends-new*))
				      (return-from file-out-dated t))))))))))


(defun make-compile-file ( l)
  (format t "~&Begin compile ~a at ~a~%" l (print-date nil))
  (dolist (v *when-compile*) (eval v))
  (setq *when-compile* nil)  (dolist (v *when-compile*) (eval v))
  (setq *when-compile* nil)
  ;;Franz excl needs pathnames quoted, and some other lisp
  ;;would not allow an apply here.  Sad.
  (eval `(compile-file ',(source l) :output-file ',(object l)
		       ,@ *cflags*))
  (format t "~&End compile ~a at ~a~%" l (print-date nil))

  )
(defun make-load-file (l) (load  (object l)))

;;these are versions which don't really compile or load files, but
;;do create a new "compiled file" and "fake load" to test date mechanism.
#+debug
(defun make-compile-file (file)
  (format t "~%Fake Compile ~a" (namestring (source file)))
    (dolist (v *when-compile*) (eval v))  (setq *when-compile* nil)
  (with-open-file (st (object file) :direction :output)
		  (format st "(print (list 'hi))")))
#+debug
(defun make-load-file (l)
  (format t "~%Fake loading ~a" (namestring(object l))))




(defun compile-files (files &optional (*force*  *force*)
			    &key (source-path source-path)
			    (object-path object-path)
			    &aux
			    (*compile* t) )
  (declare (special object-path source-path *force* show))
  (cond ((atom files)
	 (when (or *force*  (file-out-dated files))
	      (push files  *depends-new*)
	       (cond
		(show
		 (format t "~%(COMPILE-FILE ~s)" (namestring (source files))))
		(t
		 (and *compile-file-function*
		      (funcall *compile-file-function* files))
		 ))))
	((keywordp (car files))
	 (let ((fun (get (car files) 'compile)))
	   (if fun (apply fun (cdr files)))))
	(t (dolist (v files) (compile-files v *force*)))))
;;Return
(defun system-files (system &aux *files*)
  (declare (special *files*))
  (let ((sys (get system :make)))
    (get-files1 sys))
  (nreverse *files*))


(defun get-files1 (sys)
  (cond ((and sys (atom sys) )(pushnew sys *files*))
	((eq (car sys) :serial) (get-files1 (cdr sys)))
	((keywordp (car sys)))
	(t (loop for v in sys do (get-files1 v)))))
