;; This file is part of GNU Emacs.
;; Copyright (C) 1998 William F. Schelter

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.



;; By Bill Schelter wfs@math.utexas.edu

;; Completion on forms in the buffer.   Does either a line or an sexp.
;; Uses the current prompt and the beginning of what you have typed.
;; Thus If the buffer contained

;; (dbm:3) load("jo"
;; (C11) lo("ji")
;; (gdb) last
;; maxima>>4
;; /home/bil# ls 
;; then if you are at a prompt 
;; "(C15) l"  would match lo("ji")  only, not "last", not "ls" nor load("
;; and the commands with the (gdb) prompt would only match ones
;; starting with (gdb) ..


;; also if the command is a lisp sexp and this would be longer than the
;; current line, it grabs the whole thing.  sometimes we have different
;; prompts, for different programs and we dont want to confuse the input
;; from one with input for another.  Generally the prompt matches a
;; previous prompt, with numbers matching any number, and if there are
;; '/' then match anything up to a shell prompt terminator.  Note it does
;; this without additional consing or building up huge lists of inputs.


(if (boundp 'comint-mode-map)
    (define-key comint-mode-map "\ep" 'smart-complete)
  )

(if (boundp 'sshell-mode-map)
    (define-key sshell-mode-map "\ep" 'smart-complete)
   (define-key sshell-mode-map "\M-p" 'smart-complete)
  )

(defun get-match-n (i )
  (buffer-substring (match-beginning i) (match-end i)))

(defun smart-complete ()
  "Begin to type the command and then type M-p.  You will be
 offered in the minibuffer a succession of choices, which
 you can say 'n' to to get the next one, or 'y' or 'space'
 to grab the current one.

 Thus to get the last command starting with 'li' you type
 liM-py
"
  (interactive )
  (let ((point (point)) new str tem prompt)
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at sshell-prompt-pattern)
	   (setq prompt (get-match-n 0))
	   (setq str (buffer-substring (match-end 0) point)))
	  (t (error "Your prompt on this line does not match sshell-prompt-pattern")))

    (setq new (smart-complete2 prompt str))
    )
  (cond (new
	 (delete-region (setq tem (- point (length str))) point)
	 (goto-char tem)
	 (insert new)))))

(defun smart-complete2 (prompt str)
  (let ((pt (point)) found
	(pat (concat (regexp-for-this-prompt prompt)
		     "\\(" (regexp-quote str) "\\)" ))
	offered (not-yet t)
	)
    (setq bill pat)
    (while (and  not-yet
		 (re-search-backward pat nil t))
      (goto-char (match-beginning 1))
      (setq at (match-beginning 1))
      (goto-char at)
      (setq this (buffer-substring at
		  (save-excursion (end-of-line) (point))))
       (or  (member this offered)
	    (equal this str)
	    (progn (setq offered (cons this offered))
		   ;; do this so the display does not shift...
		   (goto-char pt)
		   (setq not-yet
			 (not (y-or-n-p (concat "Use: " this " "))))))
       (cond (not-yet (goto-char at) (beginning-of-line) (forward-char -1))
	     (t (setq found
		      (save-excursion
			(buffer-substring
			 at
			 (progn (goto-char at)
				(max (save-excursion
				       (end-of-line) (point))
				     (save-excursion
				       (forward-sexp 1)(point)))
				)))))))
    (or found (message "No more matches"))
    found
    ))


;; return a regexp for this prompt but with numbers replaced.

;; (defun split-string (s bag)
;;   (cond ((equal (length s) 0) '(""))
;; 	((string-match bag s)
;; 	 (if (= (match-beginning  0) 0)
;; 	    (cons "" (split-string (substring s (match-end 0)) bag))
;; 	   (cons (substring s 0 (match-beginning 0))
;; 		 (split-string (substring s (match-end 0)) bag))))
;; 	(t (cons s nil))))

;; Return a regexp which matches the current prompt, and which
;; allows things like
;; "/foo/bar# " to match  "any# "
;; "(C12) " to match  "(C1002) " but not (gdb) nor "(D12) "
;; if the prompt appears to be a pathname (ie has /) then
;; allow any beginning, otherwise numbers match numbers...
(defun regexp-for-this-prompt (prompt )
  (let ((wild (cond ((string-match "/" prompt) "[^ >#%()]+")
		    (t "[0-9]+"))))
  (let ((tem (split-string prompt wild)) (ans ""))
    (while tem
      (setq ans (concat ans (regexp-quote (car tem))))
      (cond ((cdr tem) (setq ans (concat ans wild))))
      (setq tem (cdr tem)))
    ans)))

  
(provide 'smart-complete)

  
	  
	  
    

