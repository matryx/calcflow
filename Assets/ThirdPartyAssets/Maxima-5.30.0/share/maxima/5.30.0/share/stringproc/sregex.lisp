;;;;
;;;;                                   ~*~  sregex  ~*~
;;;;
;;;;  Maxima interface to pregexp.lisp (portable regex parser, (c) Dorai Sitaram, see copyright in pregexp.lisp)
;;;;
;;;;  Version       : 1.0 (march 2008)
;;;;  Copyright     : 2008 Volker van Nek
;;;;  Licence       : GPL2
;;;;


;; regex_match_pos("needle", "hay needle stack");
;;                                                [[5, 11]]
;; regex_match("needle", "hay needle stack");
;;                                                 [needle]
;; regex_split("", "smithereens");
;;                                    [s, m, i, t, h, e, r, e, e, n, s]
;; regex_split("[,;]+", "split,pea;;;soup");
;;                                            [split, pea, soup]
;; regex_subst_first("ty", "te", "liberte");
;;                                                 liberty
;; regex_subst("ty", "te", "liberte egalite fraternite");
;;                                        liberty egality fratyrnity
;; string_to_regex("list?");
;;                                                  list\?

;; for a small tutorial and more examples see http://www.ccs.neu.edu/home/dorai/pregexp/pregexp.html


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)


($load "pregexp")


(defun $regex_match_pos (pattern str &optional (start 1) (end nil)) ;; 1-based indexing!
  (if (or (not (stringp pattern)) (not (stringp str)))
    (merror "regex_match_pos: first two arguments must be strings."))
  (or (ignore-errors ;; suppresses Lisp error outputs with internal 0-based indexing 
        (let ((pos-list 
                (if end 
                  (pregexp-match-positions pattern str (1- start) (1- end))
                  (pregexp-match-positions pattern str (1- start))))
              (pos-mlist nil))
          (if pos-list 
            (dolist (pos pos-list (cons '(mlist) (reverse pos-mlist)))
              (setq pos-mlist (cons (list '(mlist) (1+ (car pos)) (1+ (cdr pos))) pos-mlist)))
            (return-from $regex_match_pos nil))))
      (merror "regex_match_pos: improper start or end index.")))



(defun $regex_match (pattern str &optional (start 1) (end nil)) ;; 1-based indexing!
  (if (or (not (stringp pattern)) (not (stringp str)))
    (merror "regex_match_pos: first two arguments must be strings."))
  (or (ignore-errors ;; suppresses Lisp error outputs with internal 0-based indexing 
        (let ((match 
                (if end 
                  (pregexp-match pattern str (1- start) (1- end))
                  (pregexp-match pattern str (1- start)))))
          (if match 
            (cons '(mlist) match)
            (return-from $regex_match nil))))
      (merror "regex_match: improper start or end index.")))



(defun $regex_split (pattern str)
  (if (or (not (stringp pattern)) (not (stringp str)))
    (merror "regex_split: first two arguments must be strings."))
  (cons '(mlist) (pregexp-split pattern str)))


(defun $regex_subst_first (new old str)
  (if (not (and (stringp new) (stringp old) (stringp str)))
    (merror "regex_subst_first: arguments must be strings."))
  (pregexp-replace old str new))


(defun $regex_subst (new old str)
  (if (not (and (stringp new) (stringp old) (stringp str)))
    (merror "regex_subst: arguments must be strings."))
  (pregexp-replace* old str new))


(defun $string_to_regex (str)
  (if (not (stringp str))
    (merror "string_to_regex: argument must be a string."))
  (pregexp-quote str))

