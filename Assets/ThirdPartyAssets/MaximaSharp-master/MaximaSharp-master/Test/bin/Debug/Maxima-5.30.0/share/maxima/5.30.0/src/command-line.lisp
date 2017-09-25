;;;; command-line.lisp -- Application command line argument retrieval
;;;;                      and processing for Common Lisp.

;;;; Copyright (C) 2003 James F. Amundson

;;;; command-line.lisp is free software; you can redistribute it
;;;; and/or modify it under the terms of the GNU General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.

;;;; command-line.lisp is distributed in the hope that it will be
;;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;; See the GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with command-line.lisp; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, Inc., 59 Temple Place -
;;;; Suite 330, Boston, MA 02111-1307, USA.

;; Defined in maxima-package.
;; (defpackage "COMMAND-LINE"
;;   (:use "COMMON-LISP")
;;   (:nicknames "CMDLINE")
;;   (:export "CL-OPTION" "MAKE-CL-OPTION" "LIST-CL-OPTIONS" "PROCESS-ARGS"
;; 	   "GET-APPLICATION-ARGS"))

(in-package :command-line)

(defstruct cl-option
  (names nil)
  (argument nil)
  (action nil)
  (help-string nil))

(defun cl-option-description (name arg)
  (if arg
      (cond ((= (length name) 1) (format nil "~a ~a" name arg))
	    ((equal (subseq name 0 2) "--") (format nil "~a=~a" name arg))
	    (t (format nil "~a ~a" name arg)))
      name))

(defun list-cl-options (cl-option-list)
  (format t "options:~%")
  (dolist (opt cl-option-list)
    (let ((help-string (cl-option-help-string opt))
	  (names (cl-option-names opt))
	  (arg (cl-option-argument opt)))
      (format t "    ~a" (cl-option-description (first names) arg))
      (dolist (name (rest names))
	(format t ", ~a" (cl-option-description name arg)))
      (terpri)
      (if help-string
	  (format t "        ~a" help-string))
      (terpri))))

;; Old argument processing.  Leaving this here for now, but if getopts
;; works well enough, then these should be removed.
#+(or)
(progn
(defun parse-args (args cl-option-list)
  (if (null args)
      nil
      (let ((arg (pop args))
	    (arg-matched nil))
	(dolist (opt cl-option-list)
	  (when (member arg (cl-option-names opt) :test #'equal)
	    (cond ((and (cl-option-action opt) (cl-option-argument opt))
		   (funcall (cl-option-action opt) (pop args)))
		  ((cl-option-action opt)
		   (funcall (cl-option-action opt)))
		  ((cl-option-argument opt)
		   (pop args)))
	    (setf arg-matched t)
	    (return t)))
	(unless (or arg-matched (equal arg ""))
	  (format t "Warning: argument ~a not recognized~%" arg))
	(parse-args args cl-option-list))))

(defun expand-compound-arg (arg)
  (map 'list  #'(lambda (char) (concatenate 'string "-" (string char)))
       (subseq arg 1)))

(defun expand-equals-arg (arg)
  (let ((equals-position (search "=" arg)))
    (list (subseq arg 0 equals-position) (subseq arg (+ 1 equals-position)))))

(defun expand-args (args)
  (if (null args)
      nil
      (let* ((arg (car args))
	     (rest (expand-args (cdr args)))
	     (listarg (list arg)))
	(cond ((< (length arg) 2) nil)
	      ((and (equal (subseq arg 0 2) "--") (search "=" arg))
	       (setf listarg (expand-equals-arg arg)))
	      ((equal (subseq arg 0 2) "--") nil)
	      ((equal (char arg 0) #\-)
	       (if (> (length arg) 2)
		   (setf listarg (expand-compound-arg arg)))))
	(append listarg rest))))

(defun process-args (args cl-option-list)
  (parse-args (expand-args args) cl-option-list))
)

(defun process-args (args cl-option-list)
  (flet ((fixup (options)
	   ;; Massage cl-option into the format wanted by getopt.
	   ;; Basically, remove any leading dashes, and if the
	   ;; cl-option includes an argument, treat it as a required
	   ;; argument.
	   (let ((opts nil))
	     (dolist (o options)
	       (dolist (name (cl-option-names o))
		 (push (list (string-left-trim "-" name)
			     (if (cl-option-argument o)
				 :required
				 :none)
			     nil)
		       opts)))
	     (nreverse opts))))
    (let ((options (fixup cl-option-list)))
      (multiple-value-bind (non-opts opts errors)
	  (getopt:getopt args options :allow-exact-match t)
	(declare (ignore non-opts))	;non-opts ignored for now
	;; Look over all of opts and run the action
	#+nil (format t "opts = ~S~%" opts)
	(dolist (o opts)
	  ;; Try to find the corresponding cl-option.
	  (let ((cl-opt (find (car o)
			      cl-option-list
			      :test #'(lambda (desired e)
					;; Strip off any leading
					;; dashes from the option name
					;; and compare with the
					;; desired option.
					(member desired (cl-option-names e)
						:test #'equal
						:key #'(lambda (e)
							 (string-left-trim "-" e)))))))
	    #+nil (format t "Processing ~S -> ~S~%" o cl-opt)
	    (if cl-opt
		(cond ((and (cl-option-action cl-opt) (cl-option-argument cl-opt))
		       (funcall (cl-option-action cl-opt) (cdr o)))
		      ((cl-option-action cl-opt)
		       (funcall (cl-option-action cl-opt))))
		(warn "Could not find option ~S in cl-options: ~S.~%Please report this bug."
		      o cl-option-list))))
	(format t "~{Warning: argument ~A not recognized.~%~}" errors)
	;; What do we do about non-option arguments?  We just ignore them for now.
	))))


(defun get-application-args ()
  ;; -- is used to distinguish between options for a lisp implementation
  ;; and for Maxima.
  (flet ((remove-implementation-args (arglist)
           (let ((dashes (member "--" arglist :test #'equal)))
             (if dashes
                 (cdr dashes)
                 arglist))))
    (remove-implementation-args
     #+clisp
     (rest ext:*args*)
    
     #+ecl
     (rest (ext:command-args))

     #+cmu
     (if (boundp 'ext::*command-line-application-arguments*)
	 ext::*command-line-application-arguments*
	 (rest ext:*command-line-strings*))
     
     #+scl
     (rest ext:*command-line-strings*)

     #+sbcl
     (rest sb-ext:*posix-argv*)

     #+gcl
     (rest si:*command-args*)

     #+allegro
     (rest (system:command-line-arguments :application t))
      
     #+lispworks
     (rest system:*line-arguments-list*)

     #+openmcl
     (rest ccl:*command-line-argument-list*)

     #+abcl
     ext:*command-line-argument-list*)))
