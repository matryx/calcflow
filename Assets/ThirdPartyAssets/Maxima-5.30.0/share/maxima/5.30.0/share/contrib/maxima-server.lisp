;; maxima-server.lisp -- create simultaneous, independent Maxima sessions via POSIX fork
;;
;; Copyright 2007 by Robert Dodier.
;; I release this file under the terms of
;; the GNU General Public License.
;;
;; The function SERVER-RUN implements a simple Unix server:
;; listen, accept, fork.
;;
;; Actually fork is called twice and child exits immediately,
;; so the grandchild process cannot become a zombie.
;;
;; This code only works for SBCL. It might work with some modification
;; for other Lisps which have POSIX functions.
;;
;; This code is experimental and if it causes all kinds of errors,
;; that's to be expected.
;;
;; Example:
;;
;; Server:
;;
;; $ maxima
;; (%i1) load ("./maxima-server.lisp");
;; (%i2) :lisp (server-run)
;; JUST BEFORE SOCKET-ACCEPT ...
;; (etc etc log messages here)
;;
;; Client:
;;
;; $ telnet localhost 1042
;; Trying 127.0.0.1...
;; Connected to localhost.
;; Escape character is '^]'.
;; Maxima restarted.
;; (%i2) build_info ();
;; 
;; Maxima version: 5.12.0cvs
;; Maxima build date: 9:5 5/12/2007
;; host type: i686-pc-linux-gnu
;; lisp-implementation-type: SBCL
;; lisp-implementation-version: 1.0
;; 
;; (%o2) 
;; (%i3) ^]
;; telnet> quit
;; Connection closed.


(defvar listening-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
(sb-bsd-sockets:socket-bind listening-socket (sb-bsd-sockets:make-inet-address "127.0.0.1") 1042)
(sb-bsd-sockets:socket-listen listening-socket 5)

(defun server-run ()
  (loop do
    (format t "JUST BEFORE SOCKET-ACCEPT ...~%")
    (multiple-value-bind (working-socket peer-address) (sb-bsd-sockets:socket-accept listening-socket)
      (format t "ACCEPTED CLIENT; PEER-ADDRESS = ~S~%WORKING-SOCKET = ~S~%" peer-address working-socket)
  
      ; Conventional Unix hackery here:
      ; fork twice and immediate exit first child process,
      ; so that grandchild is eventually inherited by init process;
      ; thus grandchild doesn't become a zombie.
  
      (let ((child-pid (sb-posix:fork)))
        (if (eq child-pid 0)
          (let ((grandchild-pid (sb-posix:fork)))
            (if (eq grandchild-pid 0)
              (progn
                ; Grandchild process: I execute the Maxima session here.
                (let*
                  ((working-stream (sb-bsd-sockets:socket-make-stream working-socket :input t :output t))
                   (*standard-input* working-stream)
                   (*standard-output* working-stream))
                  (handler-case (cl-user::run)
                    (error nil t)))
                (format t "SERVER-RUN RETURNED (OK, BUT USUALLY SERVER-RUN CALLS EXIT)~%")
                (sb-bsd-sockets:socket-close working-socket))
              (progn
                ; Child process: I exit immediately.
                (format t "CHILD: IMMEDIATE EXIT; GRANDCHILD PID = ~S~%" grandchild-pid)
                (sb-ext:quit))))
          (progn
            (format t "PARENT: WAIT FOR CHILD; CHILD PID = ~S~%" child-pid)
            (sb-posix:waitpid child-pid 0)))))))
  
