#+lispworks
(setq hcl:*packages-for-warn-on-redefinition*
    (remove-if (lambda (package-name)
                 (member package-name
                         '("HARLEQUIN-COMMON-LISP"
                           ;"CL-USER"
                           )
                         :test #'string-equal))
               *packages-for-warn-on-redefinition*))
#+lispworks (in-package "HARLEQUIN-COMMON-LISP")
#+lispworks (defun getenv (x) (LW:ENVIRONMENT-VARIABLE x))
#+lispworks (in-package "CL-USER")

(load "../lisp-utils/defsystem.lisp")
#+ecl (load "maxima-package.lisp")
#+ecl
(compile 'maxima::make-unspecial
	 '(lambda (s)
	   (when (symbolp s)
	     (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
	     (ffi::c-inline (s) (:object) :object
			    "((#0)->symbol.stype = stp_ordinary, #0)"
			    :one-liner t))))

(defun maxima-compile ()
  (mk:oos "maxima" :compile))
(defun maxima-load ()
  (mk:oos "maxima" :load))

(defun maxima-dump ()
  #+clisp (ext:saveinitmem "binary-clisp/maxima.mem" :init-function (function cl-user::run))
  #+sbcl (sb-ext:save-lisp-and-die "binary-sbcl/maxima.core" :toplevel #'cl-user::run)
  #+gcl (si:save-system "binary-gcl/maxima")
  #+cmu (extensions:save-lisp "binary-cmucl/maxima.core" :init-function 'cl-user::run)
  #+scl (extensions:save-lisp "binary-scl/maxima.core" :init-function 'cl-user::run)
  #+allegro (excl:dumplisp :name "binary-acl/maxima.dxl")
  #+lispworks (hcl:save-image "binary-lispworks/maxima" :restart-function 'cl-user::run)
  #+ccl (ccl:save-application "binary-openmcl/maxima" :toplevel-function 'cl-user::run))


