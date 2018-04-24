;; noninteractive.lisp -- some functions to be used by noninteractive.mac
;; copyright 2007 by Robert Dodier
;; I release this file under the terms of the GNU General Public License.

;; Redefine MERROR to throw something.
(defun merror (s &rest l) ($throw `((merror) ,s ,@l)))

(defmspec $assuming (e)
  (let*
    ((args (margs e))
     (assumptions (mapcar #'meval (rest (first args)))))
    (meval `(($assume) ,@assumptions))
    (unwind-protect
      (first (last (mapcar #'meval (rest args))))
      (meval `(($forget) ,@assumptions)))))

;; Remove functions and variables defined at Maxima level
;; so that kill does not affect noninteractive.
;; Unfortunately, this function has to revised if any more
;; functions or variables are defined. 
;; Maybe there should be a way to mark a function as unkillable
;; when it is defined. Just a thought.

(defun $delete_noninteractive_stuff_from_infolists ()
  (declare (special $values $functions))
  (setq $values (delete '|$within_MEVAL1| $values))
  (setq $functions (delete '((|$meval1|)) $functions :test #'equal))
  (setq $functions (delete '(($ENUMERATE_CASES) |$l%|) $functions :test #'equal))
  (setq $functions (delete '(($INTERLEAVE) |$l1| |$l2|) $functions :test #'equal))
  '$done)
