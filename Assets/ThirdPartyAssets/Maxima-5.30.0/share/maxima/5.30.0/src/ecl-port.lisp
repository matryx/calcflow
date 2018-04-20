(in-package :maxima)

(eval-when (:execute)
  (compile 'maxima::make-unspecial
	   '(lambda (s)
	     (when (symbolp s)
	       (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
	       (ffi::c-inline (s) (:object) :object
		     "(#0)->symbol.stype &= ~stp_special;"
		     :one-liner nil)
	       s))))

(eval-when (:load-toplevel)
  (defun maxima::make-unspecial (s)
    (when (symbolp s)
      (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
      (ffi::c-inline (s) (:object) :object
		     "(#0)->symbol.stype &= ~stp_special;"
		     :one-liner nil)
      s)))

(si::trap-fpe 'floating-point-underflow nil)

