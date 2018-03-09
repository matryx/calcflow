;; Connect Maxima to a socket which has been opened by
;; some third party, typically a GUI program which supplies
;; input to Maxima.
;; Note that this code DOES NOT create a Maxima server:
;; Maxima is the client!

(in-package :maxima)

#+(or ecl sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require 'asdf)		    ;not needed here for a recent SBCL
  #+sbcl (require 'sb-posix)
  (require 'sb-bsd-sockets))

(defvar $in_netmath nil)
(defvar $show_openplot t)
(defvar *socket-connection*)

(defun setup-client (port &optional (host "localhost"))
  (let* ((sock (open-socket host port)))
    #+gcl (setq si::*sigpipe-action* 'si::bye)
    (setq *socket-connection* sock)
    #+ecl (setq *old-stdin* *standard-input*
		*old-stdout* *standard-output*
		*old-stderr* *error-output*
		*old-term-io* *terminal-io*
		*old-debug-io* *debug-io*)
    (setq *standard-input* sock)
    (setq *standard-output* sock)
    (setq *error-output* sock)
    (setq *terminal-io* sock)
    (setq *trace-output* sock)
    (format t "pid=~a~%" (getpid))
    (force-output sock)
    (setq *debug-io* sock))
  (values))

(defun close-client ()
  #+ecl (setq *standard-input* *old-stdin*
	      *standard-output* *old-stdout*
	      *error-output* *old-stderr*
	      *terminal-io* *old-term-io*
	      *debug-io* *old-debug-io*)
  #+ecl (close *socket-connection*))

;;; from CLOCC: <http://clocc.sourceforge.net>
(defun open-socket (host port &optional bin)
  "Open a socket connection to `host' at `port'."
  (declare (type (or integer string) host) (fixnum port) (type boolean bin))
  (let ((host (etypecase host
                (string host)
                (integer (hostent-name (resolve-host-ipaddr host))))))
    #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
    #+clisp (socket:socket-connect port host :element-type
				   (if bin '(unsigned-byte 8) 'character))
    #+scl (sys:make-fd-stream (ext:connect-to-inet-socket host port)
			      :input t :output t :element-type
			      (if bin '(unsigned-byte 8) 'character))
    #+cmu (sys:make-fd-stream (ext:connect-to-inet-socket host port)
			      :input t :output t :element-type
			      (if bin '(unsigned-byte 8) 'character)
			      #+unicode :external-format #+unicode :utf-8)
    #+(or ecl sbcl) (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
					:type :stream :protocol :tcp)))
	     (sb-bsd-sockets:socket-connect
	      socket (sb-bsd-sockets:host-ent-address
		      (sb-bsd-sockets:get-host-by-name host)) port)
	     (sb-bsd-sockets:socket-make-stream
	      socket :input t :output t :buffering (if bin :none :line)
	      :element-type (if bin '(unsigned-byte 8) 'character)))
    #+gcl (si::socket port :host host)
    #+lispworks (comm:open-tcp-stream host port :direction :io :element-type
                                      (if bin 'unsigned-byte 'base-char))
    #+ccl (ccl::make-socket :remote-host host :remote-port port)
    #-(or allegro clisp cmu scl sbcl gcl lispworks ecl ccl)
    (error 'not-implemented :proc (list 'open-socket host port bin))))



(defun start-client (port &optional (host "localhost"))
  (format t (intl:gettext "Connecting Maxima to server on port ~a~%") port)
  (setq $in_netmath t)
  (setq $show_openplot nil)
  (setup-client port host))

#-gcl
(defun getpid-from-environment ()
  (handler-case
      (values (parse-integer (maxima-getenv "PID")))
    ((or type-error parse-error) () -1)))

;;; For gcl, getpid imported from system in maxima-package.lisp
#-gcl
(defun getpid ()
#+clisp (os:process-id)
#+(or cmu scl) (unix:unix-getpid)
#+sbcl (sb-unix:unix-getpid)
#+gcl (system:getpid)
#+openmcl (ccl::getpid)
#+lispworks (system::getpid)
#+ecl (si:getpid)
#+ccl (ccl::getpid)
#-(or clisp cmu scl sbcl gcl openmcl lispworks ecl ccl) (getpid-from-environment)
)

#+(or gcl clisp cmu scl sbcl lispworks ecl)
(defun xchdir (w)
  #+clisp (ext:cd w)
  #+gcl (si::chdir w)
  #+(or cmu scl) (unix::unix-chdir w)
  #+sbcl (sb-posix:chdir w)
  #+lispworks (hcl:change-directory w)
  #+ecl (si:chdir w)
  )
