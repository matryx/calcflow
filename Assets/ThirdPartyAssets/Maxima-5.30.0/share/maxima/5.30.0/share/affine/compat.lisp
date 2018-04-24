(in-package :maxima)

;; (format t "~%The functions ~/maxima::tilde-q-fsh/ are inverses"
;;	(st-rat #$[x+1,1/(x+1)]$))
;; should print: The functions x+1 and 1/(x+1) are inverses

;; See the last part of polyb for the definition of FSH.

(defun tilde-q-fsh (stream arg colonp at-p) ;atp is special...
  (declare (ignore colonp at-p))
  (fsh arg stream))

