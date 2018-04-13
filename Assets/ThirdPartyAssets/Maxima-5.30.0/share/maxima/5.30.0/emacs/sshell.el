
;; Run subshell under Emacs
;; Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.
;; Modifications by William Schelter
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; The following is a "simple shell" much like the one in version 18
;; of emacs.   Unfortunately cmint breaks most code which tries to use
;; the shell mode, and is rather complex.

;; This mode uses a better completion mechanism (smart-complete.el),
;; in that it should
;; find the input you really want with your typing less keystrokes,
;; and easier keystrokes to type





(defvar last-input-start nil
  "In a sshell-mode buffer, marker for start of last unit of input.")
(defvar last-input-end nil
  "In a sshell-mode buffer, marker for end of last unit of input.")

(defvar sshell-mode-map nil)

(defvar sshell-directory-stack nil
  "List of directories saved by pushd in this buffer's sshell.")

(defvar sshell-popd-regexp "popd"
  "*Regexp to match subsshell commands equivalent to popd.")

(defvar sshell-pushd-regexp "pushd"
  "*Regexp to match subsshell commands equivalent to pushd.")

(defvar sshell-cd-regexp "cd"
  "*Regexp to match subsshell commands equivalent to cd.")

(defvar explicit-sshell-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior sshell.")


;In loaddefs.el now.
(defconst sshell-prompt-pattern
 "\\(^\\|\n\\)[^ >]*[>$)%#:][>]*[ ]*"
  "*Regexp used by Newline command to match subsshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not reexecuted.")

(defun sshell-mode ()
  "Major mode for interacting with an inferior sshell.
Sshell name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{sshell-mode-map}

Entry to this mode calls the value of sshell-mode-hook with no args,
if that value is non-nil.

cd, pushd and popd commands given to the sshell are watched
by Emacs to keep this buffer's default directory
the same as the sshell's working directory.
Variables sshell-cd-regexp, sshell-pushd-regexp and sshell-popd-regexp
are used to match these command names.

You can send text to the sshell (or its subjobs) from other buffers
using the commands process-send-region, process-send-string
and lisp-send-defun."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sshell-mode)
  (setq mode-name "Sshell")
  (setq mode-line-process '(": %s"))
  (use-local-map sshell-mode-map)
  (make-local-variable 'sshell-directory-stack)
  (setq sshell-directory-stack nil)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (run-hooks 'sshell-mode-hook))

(if sshell-mode-map
    nil
  (setq sshell-mode-map (make-sparse-keymap))
  (define-key sshell-mode-map "\t" 'sshell-complete-filename)
  (define-key sshell-mode-map "\C-m" 'sshell-send-input)
  (define-key sshell-mode-map "\C-c\C-d" 'sshell-send-eof)
  (define-key sshell-mode-map "\C-c\C-u" 'kill-sshell-input)
  (define-key sshell-mode-map "\C-c\C-w" 'backward-kill-word)
  (define-key sshell-mode-map "\C-c\C-c" 'interrupt-sshell-subjob)
  (define-key sshell-mode-map "\C-c\C-z" 'stop-sshell-subjob)
  (define-key sshell-mode-map "\C-c\C-\\" 'quit-sshell-subjob)
  (define-key sshell-mode-map "\C-c\C-o" 'kill-output-from-sshell)
  (define-key sshell-mode-map "\C-c\C-r" 'show-output-from-sshell)
  (define-key sshell-mode-map "\C-c\C-y" 'copy-last-sshell-input))


(defun sshell-complete-filename ()
  (interactive)
  (let* ((p (point))  tem beg
	 (ff
	  (save-excursion
	    (skip-chars-backward "[a-z---_0-9$/A-Z~#.]")
	    (buffer-substring (setq beg (point)) p))))
    (setq dir (or (file-name-directory ff) default-directory))
    (setq file (file-name-nondirectory ff))
    (cond ((and (setq tem (file-name-completion (or file "") dir))
		(not (equal tem file)))
	   (cond ((eq tem t))
		 (t
		  (delete-region beg p)
		  (insert (concat dir tem)))))
	  (t
	   (let ((lis (file-name-all-completions file dir)))
	     (with-output-to-temp-buffer "*completions*"
	       (display-completion-list	lis))
	     )))))

(defvar explicit-csh-args
  (if (eq system-type 'hpux)
      ;; -T persuades HP's csh not to think it is smarter
      ;; than us about what terminal modes to use.
      '("-i" "-T")
    '("-i"))
  "Args passed to inferior sshell by M-x sshell, if the sshell is csh.
Value is a list of strings, which may be nil.")


(defun sshell ()
  "Run an inferior sshell, with I/O through buffer *sshell*.
If buffer exists but sshell process is not running, make new sshell.
Program used comes from variable explicit-sshell-file-name,
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file ~/.emacs_SHELLNAME exists, it is given as initial input
 (Note that this may lose due to a timing error if the sshell
  discards input when it starts up.)
The buffer is put in sshell-mode, giving commands for sending input
and controlling the subjobs of the sshell.  See sshell-mode.
See also variable sshell-prompt-pattern.

The sshell file name (sans directories) is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the sshell.
Otherwise, one argument `-i' is passed to the sshell.

Note that many people's .cshrc files unconditionally clear the prompt.
If yours does, you will probably want to change it."
  (interactive)
  (let* ((prog (or explicit-sshell-file-name
		   (getenv "ESHELL")
		   (getenv "SHELL")
		   "/bin/sh"))		     
	 (name (file-name-nondirectory prog)))
    (switch-to-buffer
     (apply 'make-sshell "shell" prog
	    (if (file-exists-p (concat "~/.emacs_" name))
		(concat "~/.emacs_" name))
	    (let ((symbol (intern-soft (concat "explicit-" name "-args"))))
	      (if (and symbol (boundp symbol))
		  (symbol-value symbol)
		'("-i")))))))

(defun make-sshell (name program &optional startfile &rest switches)
  (let ((buffer (get-buffer-create (concat "*" name "*")))
	proc status size)
    (setq proc (get-buffer-process buffer))
    (if proc (setq status (process-status proc)))
    (save-excursion
      (set-buffer buffer)
      ;;    (setq size (buffer-size))
      (if (memq status '(run stop))
	  nil
	(if proc (delete-process proc))
	(setq proc (apply 'start-process name buffer
			   (or program explicit-sshell-file-name
			      (getenv "ESHELL")
			      (getenv "SHELL")
			      "/bin/sh")
			  switches))
	
	(cond (startfile
	       ;;This is guaranteed to wait long enough
	       ;;but has bad results if the sshell does not prompt at all
	       ;;	     (while (= size (buffer-size))
	       ;;	       (sleep-for 1))
	       ;;I hope 1 second is enough!
	       (sleep-for 1)
	       (goto-char (point-max))
	       (insert-file-contents startfile)
	       (setq startfile (buffer-substring (point) (point-max)))
	       (delete-region (point) (point-max))
	       (process-send-string proc startfile)))
	(setq name (process-name proc)))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (sshell-mode))
    buffer))

(defvar sshell-set-directory-error-hook 'ignore
  "Function called with no arguments when sshell-send-input
recognizes a change-directory command but gets an error
trying to change Emacs's default directory.")

(defun sshell-send-input ()
  "Send input to subsshell.
At end of buffer, sends all text after last output
 as input to the subsshell, including a newline inserted at the end.
When not at end, copies current line to the end of the buffer and sends it,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of sshell-prompt-pattern if possible.
This regexp should start with \"^\"."
  (interactive)
  (or (get-buffer-process (current-buffer))
      (error "Current buffer has no process"))
  (end-of-line)
  (if (eobp)
      (progn
	(move-marker last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert ?\n)
	(move-marker last-input-end (point)))
    (beginning-of-line)
    ;; Exclude the sshell prompt, if any.
    (re-search-forward sshell-prompt-pattern
		       (save-excursion (end-of-line) (point))
		       t)
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (move-marker last-input-start (point))
      (insert copy)
      (move-marker last-input-end (point))))
  ;; Even if we get an error trying to hack the working directory,
  ;; still send the input to the subsshell.
  (condition-case ()
      (save-excursion
	(goto-char last-input-start)
	(sshell-set-directory))
    (error (funcall sshell-set-directory-error-hook)))
  (let ((process (get-buffer-process (current-buffer)))
	(s (buffer-substring last-input-start last-input-end))
	)
    ;; avoid sending emacs's idea of what an international character
    ;; set string is to a subprocess..
    (if (fboundp 'string-make-unibyte)
	(setq s (string-make-unibyte s)))
    (process-send-string process s)
    (set-marker (process-mark process) (point))))

;;;  If this code changes (sshell-send-input and sshell-set-directory),
;;;  the customization tutorial in
;;;  info/customizing-tutorial must also change, since it explains this
;;;  code.  Please let marick@gswd-vms.arpa know of any changes you
;;;  make. 


(defun sshell-set-directory ()
  (cond ((and (looking-at sshell-popd-regexp)
	      (memq (char-after (match-end 0)) '(?\; ?\n)))
	 (if sshell-directory-stack
	     (progn
	       (cd (car sshell-directory-stack))
	       (setq sshell-directory-stack (cdr sshell-directory-stack)))))
	((looking-at sshell-pushd-regexp)
	 (cond ((memq (char-after (match-end 0)) '(?\; ?\n))
		(if sshell-directory-stack
		    (let ((old default-directory))
		      (cd (car sshell-directory-stack))
		      (setq sshell-directory-stack
			    (cons old (cdr sshell-directory-stack))))))
	       ((memq (char-after (match-end 0)) '(?\  ?\t))
		(let (dir)
		  (skip-chars-forward "^ ")
		  (skip-chars-forward " \t")
		  (if (file-directory-p
			(setq dir
			      (expand-file-name
				(substitute-in-file-name
				  (buffer-substring
				    (point)
				    (progn
				      (skip-chars-forward "^\n \t;")
				      (point)))))))
		      (progn
			(setq sshell-directory-stack
			      (cons default-directory sshell-directory-stack))
			(cd dir)))))))
	((looking-at sshell-cd-regexp)
	 (cond ((memq (char-after (match-end 0)) '(?\; ?\n))
		(cd (getenv "HOME")))
	       ((memq (char-after (match-end 0)) '(?\  ?\t))
		(let (dir)
		  (forward-char 3)
		  (skip-chars-forward " \t")
		  (if (file-directory-p
			(setq dir
			      (expand-file-name
				(substitute-in-file-name
				  (buffer-substring
				    (point)
				    (progn
				      (skip-chars-forward "^\n \t;")
				      (point)))))))
		      (cd dir))))))))
  
(defun sshell-send-eof ()
  "Send eof to subsshell (or to the program running under it)."
  (interactive)
  (process-send-eof))

(defun kill-output-from-sshell ()
  "Kill all output from sshell since last input."
  (interactive)
  (goto-char (point-max))
  (beginning-of-line)
  (kill-region last-input-end (point))
  (insert "*** output flushed ***\n")
  (goto-char (point-max)))

(defun show-output-from-sshell ()
  "Display start of this batch of sshell output at top of window.
Also put cursor there."
  (interactive)
  (set-window-start (selected-window) last-input-end)
  (goto-char last-input-end))

(defun copy-last-sshell-input ()
  "Copy previous sshell input, sans newline, and insert before point."
  (interactive)
  (insert (buffer-substring last-input-end last-input-start))
  (delete-char -1))

(defun interrupt-sshell-subjob ()
  "Interrupt this sshell's current subjob."
  (interactive)
  (interrupt-process nil t))

(defun kill-sshell-subjob ()
  "Send kill signal to this sshell's current subjob."
  (interactive)
  (kill-process nil t))

(defun quit-sshell-subjob ()
  "Send quit signal to this sshell's current subjob."
  (interactive)
  (quit-process nil t))

(defun stop-sshell-subjob ()
  "Stop this sshell's current subjob."
  (interactive)
  (stop-process nil t))

(defun kill-sshell-input ()
  "Kill all text since last stuff output by the sshell or its subjobs."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (point)))
(require 'smart-complete)

(provide 'sshell)