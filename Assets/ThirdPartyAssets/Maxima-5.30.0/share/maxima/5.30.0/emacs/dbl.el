;; Run gcl,maxima,gdb etc under Emacs all possibly all in one buffer.
;;  
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

;; Description of DBL interface:

;; A facility is provided for the simultaneous display of the source code
;; in one window, while using dbl to step through a function in the
;; other.  A small arrow in the source window, indicates the current
;; line.

;; Starting up:

;; In order to use this facility, invoke the command DBL to obtain a
;; shell window with the appropriate command bindings.  You will be asked
;; for the name of a file to run.  Dbl will be invoked on this file, in a
;; window named *dbl-foo* if the file is foo.

;; M-s steps by one line, and redisplays the source file and line.

;; You may easily create additional commands and bindings to interact
;; with the display.  For example to put the dbl command next on \M-n
;; (def-dbl :next "\M-n")

;; This causes the emacs command dbl-next to be defined, and runs
;; dbl-display-frame after the command.

;; dbl-display-frame is the basic display function.  It tries to display
;; in the other window, the file and line corresponding to the current
;; position in the dbl window.  For example after a dbl-step, it would
;; display the line corresponding to the position for the last step.  Or
;; if you have done a backtrace in the dbl buffer, and move the cursor
;; into one of the frames, it would display the position corresponding to
;; that frame.

;; dbl-display-frame is invoked automatically when a filename-and-line-number
;; appears in the output.


(require 'sshell)
(require 'smart-complete)
(define-key sshell-mode-map "\ep" 'smart-complete)
(define-key sshell-mode-map "\M-p" 'smart-complete)
(require 'gcl)
(autoload 'maxima-mode "maxima-mode" "Major mode for editing maxima code and interacting with debugger" t)
(autoload 'gcl-mode "gcl" "Major mode for editing maxima code and interacting with debugger" t)
(or (rassoc 'maxima-mode auto-mode-alist)
(setq auto-mode-alist (cons '("\\.ma?[cx]\\'" . maxima-mode) auto-mode-alist))
)
(or (rassoc 'gcl-mode auto-mode-alist)
(setq auto-mode-alist (cons '("\\.li?sp\\'" . gcl-mode) auto-mode-alist))
)

(defvar dbl-prompt-pattern
   "\\(^\\|\n\\)[^ >]*[>$)%#:][>]*[ ]*"
  ; "(^|\n)\\[^ >]*[>$)%#:][>]*[ ]*+"
  "A regexp to recognize the prompt for dbl or dbl+.")
;

(defvar downcase-filenames-for-dbl
  (string-match "nt[45]" system-configuration)
  "Force the case to be lower when sending a break command" 
  )

(defvar dbl-subshell-switches
  (list "bash"  (if (string-match "nt[45]" system-configuration) '("--noediting" "-i")  '("-i"))
	)
  "Alternating list of regexp for the shell name, and list of switches to pass"
  )

(defvar dbl-filter-accumulator nil)
(defvar dbl-mode-map nil
  "Keymap for dbl-mode.")

(if dbl-mode-map
   nil
  (setq dbl-mode-map (copy-keymap sshell-mode-map))
  (define-key dbl-mode-map "\C-cl" 'dbl-find-and-display-line)
  )

(define-key ctl-x-map " " 'dbl-break)
;(define-key ctl-x-map "&" 'send-dbl-command)

;;Of course you may use `def-dbl' with any other dbl command, including
;;user defined ones.   

(defmacro def-dbl (name key &optional doc)
  (let* ((fun (intern (format "dbl-%s" (read name))))
	 )
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'dbl-call name 'arg))
	  (list 'define-key 'dbl-mode-map key  (list 'quote fun)))))

(def-dbl ":step %p"   "\M-s" "Step one source line with display")
(def-dbl ":step %p"   "\C-c\C-s" "Step one source line with display")
(def-dbl ":stepi %p"  "\C-c\t" "Step one instruction with display")
(def-dbl ":next %p"   "\M-n" "Step one source line (skip functions)")
(def-dbl ":next %p"   "\C-c\C-n" "Step one source line (skip functions)")
(def-dbl ":r"   "\M-c" "Continue with display")

(def-dbl ":finish" "\C-c\C-f" "Finish executing current function")
(def-dbl ":up %p"     "\C-cu"   "Go up N stack frames (numeric arg) with display")
(def-dbl ":down %p"   "\C-cd"   "Go down N stack frames (numeric arg) with display")


(defun dbl-mode ()
  "Major mode for interacting with an inferior Lisp or Maxima process.
It is like an ordinary shell, except that it understands certain special
redisplay commands sent by the process, such as redisplay a source file
in the other window, positioning a little arrow `==>', at a certain
line, typically the line where you are stopped in the debugger.

It uses completion based on the form of your current prompt, allowing
you to keep separate the commands you type at the debugger level and
the lisp or maxima level.

The source files should be viewed using gcl mode for lisp, and maxima-mode
for maxima.  


\\{dbl-mode-map}

\\[dbl-display-frame] displays in the other window
the last line referred to in the dbl buffer.

\\[dbl-:step] and \\[dbl-:next] in the dbl window,
call dbl to step and next and then update the other window
with the current file and position.
o
If you are in a source file, you may select a point to break
at, by doing \\[dbl-break].

Commands:
Many commands are inherited from shell mode. 
Additionally we have:

\\[dbl-display-frame] display frames file in other window
\\[dbl-:step] advance one line in program
\\[dbl-:next] advance one line in program (skip over calls).
\\[send-dbl-command] used for special printing of an arg at the current point.
C-x SPACE sets break point at current line.

You may also enter keyword break commands.

:a  show-break-variables
:b  simple-backtrace
:bds  break-bds
:bl  break-locals
:blocks  break-blocks
:break  insert a break point here
:bs  break-backward-search-stack
:bt  dbl-backtrace
:c  break-current
:delete  (lambda (&rest l) (iterate-over-bkpts l delete) (values))
:disable  [n1 .. nk] disable break points. [see :info :bkpt]
:down [n]  move n frames down
:enable  [n1 n2 ..nk] enable break points
:env  describe-environment
:fr  [n] show this frame
:fs  break-forward-search-stack
:functions  break-functions
:go  break-go
:h  break-help
:help  break-help
:ihs  ihs-backtrace
:info :bkpt show break points.
:loc  loc
:m  break-message
:n  break-next
:next  step-next
:p  break-previous
:q  break-quit
:r  resume
:resume  (lambda () resume)
:s  search-stack
:step  step-into
:t  throw-macsyma-top
:up  move up one frame
:vs  break-vs

"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'dbl-mode)
  (setq mode-name "Inferior Dbl")
  (setq mode-line-process '(": %s"))
  (use-local-map dbl-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'dbl-last-frame)
  (setq dbl-last-frame nil)
  (make-local-variable 'dbl-last-frame-displayed-p)
  (setq dbl-last-frame-displayed-p t)
  (make-local-variable 'dbl-delete-prompt-marker)
  (setq dbl-delete-prompt-marker nil)
  (make-local-variable 'dbl-filter-accumulator)
  (setq dbl-filter-accumulator nil)
  (make-local-variable 'shell-prompt-pattern)
  (setq shell-prompt-pattern dbl-prompt-pattern)
  (run-hooks 'sshell-mode-hook 'dbl-mode-hook))

(defvar current-dbl-buffer nil)

(defvar dbl-command-name (if (file-exists-p "/bin/bash") "/bin/bash"
			   "/bin/sh")
  "Pathname for executing dbl.")


(defun dbl (p)
 
 "Makes a dbl buffer, suitable for running an inferior
  gcl.  You are prompted for a name for the buffer.  After the shell
  starts you should start up your lisp program (eg gcl).  The bufferd
  has special keybindings for stepping and viewing sources.  Enter the
  debug loop with (si::dbl) or :dbl in a debug loop.  "

  (interactive "p")
  
  (let ( tem
	(dir default-directory)
	;; important for winnt version of emacs
	(binary-process-input t)
	(binary-process-output nil)
	switches
	(name (concat "dbl" (if (equal p 1) "" p) ""))
	)
    
    (switch-to-buffer (concat "*" name "*"))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (let ((tem dbl-subshell-switches) switches)
      (while tem
	(cond ((string-match (car tem) dbl-command-name)
	       (setq switches (nth 1 tem)) (setq tem nil))
	      (t (setq tem (nthcdr 2 tem)))))
      (apply 'make-sshell name dbl-command-name nil switches))
    (dbl-mode)
    (make-local-variable 'sshell-prompt-pattern)
    (setq sshell-prompt-pattern dbl-prompt-pattern)
    (goto-char (point-min))
    (insert "
Welcome to DBL a Debugger for Lisp, Maxima, Gdb and others.

You start your program as usually would in a shell.  For Lisp and
Maxima the debugger commands begin with a ':', and there is
completion.  Typing ':' should list all the commands.  In GCL these
are typed when in the debugger, and in Maxima they may be typed at any
time.  To see the wonderful benefits of this mode, type C-h m.

Note you may also use this mode to run gdb.  In fact I often debug
MAXIMA over GCL using gdb, thus having three debuggers at once.
To run gdb and enable the automatic line display, you must supply
the `--fullname' keyword as in:

 gdb your-file  --fullname
")
    (goto-char (point-max))
    (set-process-filter (get-buffer-process (current-buffer)) 'dbl-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'dbl-sentinel)
    (dbl-set-buffer)))

(defun dbl-set-buffer ()
  (cond ((eq major-mode 'dbl-mode)
	(setq current-dbl-buffer (current-buffer)))))

;; This function is responsible for inserting output from DBL
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that DBL prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun dbl-filter (proc string)
  (let ((inhibit-quit t))
    (set-buffer (process-buffer proc))
    (goto-char (point-max))
    (insert string)
    (goto-char (point-max))
    ))


(defun dbl-filter (proc string)
  (let ((inhibit-quit t))
    (if dbl-filter-accumulator
	(dbl-filter-accumulate-marker proc
				      (concat dbl-filter-accumulator string))
	(dbl-filter-scan-input proc string))
    ))


(defun dbl-filter-accumulate-marker (proc string)
  (setq dbl-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
              (progn
		(setq me string)
		(cond ((string-match
			"\032\032\\([A-Za-z]?:?[^:]*\\):\\([0-9]*\\):[^\n]+\n"
			string)
		       (setq dbl-last-frame
			      (cons
			      (match-string 1 string)
			      (string-to-int  (match-string 2 string))))
		       
		       (cond ((equal (cdr dbl-last-frame)  0)
			      ;(message "got 0")
			      ;(sit-for 1)
			      (setq overlay-arrow-position nil)
			      (setq 	dbl-last-frame nil)
			     )
			     (t   (setq dbl-last-frame-displayed-p nil))
		       )))

		  (dbl-filter-scan-input proc
					 (substring string (1+ end))))
	      (setq dbl-filter-accumulator string)))
	(dbl-filter-insert proc "\032")
	(dbl-filter-scan-input proc (substring string 1)))
    (setq dbl-filter-accumulator string)))

(defun dbl-filter-scan-input (proc string)
  (if (equal string "")
      (setq dbl-filter-accumulator nil)
      (let ((start (string-match "\032" string)))
	(if start
	    (progn
	      ;; to do fix this so that if dbl-last-frame
	      ;; changed, then set the current  text property..
	      ;;
		(dbl-filter-insert proc (substring string 0 start))
		
		(dbl-filter-accumulate-marker proc
						 (substring string start))
		)
	    (dbl-filter-insert proc string)))))

(defun dbl-filter-insert (proc string)
  (let (moving
	output-after-point 
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer proc))
    ;; test to see if we will move the point.   We want that the
    ;; window-point of the buffer, should be equal to process-mark. 
    (setq moving (>= (window-point (get-buffer-window (process-buffer proc)))
		     (- (process-mark proc) 0)))
    (setq output-after-point (< (point) (process-mark proc)))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (setq start (point))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	 ; (setq bill (cons (list 'hi (process-mark proc) (marker-position (process-mark proc)) (point)) bill))
	  (dbl-maybe-delete-prompt)
	  ;; Check for a filename-and-line number.
	  (dbl-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-line-number appears.
	   t)
	  )
      (if moving
	  (set-window-point
	   (get-buffer-window (process-buffer proc))
	   (process-mark proc)))
      (set-buffer old-buffer))
    ))

(defun dbl-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the dbl buffer.
	     (set-buffer obuf))))))


(defun dbl-refresh ()
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive)
  (redraw-display)
  (dbl-display-frame))

(defun dbl-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from DBL.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (dbl-set-buffer)
  (and dbl-last-frame (not nodisplay)
       (or (not dbl-last-frame-displayed-p) (not noauto))
       (progn (dbl-display-line (car dbl-last-frame) (cdr dbl-last-frame))
	      (setq dbl-last-frame-displayed-p t))))

	     
;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.



(defun dbl-find-file (file)
  (cond ((file-exists-p file)
	 (find-file-noselect file))
	((get-buffer file))
	(t (find-file-noselect file))))

(defvar dbl-dirs  nil)

(defun search-path (file dirs)
  (let ((paths (symbol-value dirs))
	true-file)
    (cond ((file-exists-p file) (setq true-file file))
	  (t
	   (while paths
	     (let ((tem (expand-file-name file (or (car paths) default-directory))))
	       (if  (file-exists-p tem) (setq true-file tem))
	       (setq paths (cdr paths))))))
   
   (cond (true-file)
	 (t (setq paths (symbol-value dirs))
	  (set dirs
		  (append paths
			  (list (file-name-directory
				 (read-file-name
				  (format "%s = %s, add path :" dirs paths))))))
	     (search-path file dirs)))))


(defun dbl-find-line ()
  "If the current buffer has a process, then look first for a file-line
property, and if none, then search for a regexp.
If a non process buffer, just return current file and line number.
"
  (interactive)
  (save-excursion
    (end-of-line)
    (cond ((get-buffer-process (current-buffer))
	   (cond
	    ((save-excursion
	       (beginning-of-line)
		(get-text-property (point) 'file-line)))
	    ((progn (end-of-line) (re-search-backward " \\([^: ]+\\):\\([0-9]+\\)" 300 nil))
	     (setq file (buffer-substring (match-beginning 1) (match-end 1)))
	     (setq line (buffer-substring (match-beginning 2) (match-end 2)))
	     (setq line (read line))
	     (and (integerp line)
		  (setq file (search-path file 'dbl-dirs))
		  (list file line)))))
	  (t (list (buffer-file-name) (+ 1 (count-lines (point))))))))

(defun dbl-find-and-display-line ()
  (interactive)
  (let ((res (dbl-find-line)))
    (and res (apply 'dbl-display-line res))))

(defun dbl-display-line (true-file line)
  (let* ((buffer (dbl-find-file true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(defvar dbl-gdb-command-alist '((":step %p" . "step %p")
				(":next %p" . "next %p")
				(":stepi" . "stepi %p")
				(":r" . "r")
				(":finish" . "finish")
				(":up %p" . "up %p")
				( ":down %p" . "down %p")))

(defun dbl-call (command numeric)
  "Invoke dbl COMMAND displaying source in other window."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (let (com)
      (cond ((or (looking-at "(gdb")
		 (member major-mode '(c-mode c++-mode)))
	     (if (setq com (assoc  command dbl-gdb-command-alist))
		 (setq command (cdr com))))))

    
	    ;; to do put in hook here to recognize whether at
	    ;; maxima or lisp level.

  (setq command (dbl-subtitute-% command numeric))
  (goto-char (point-max))
  (setq dbl-delete-prompt-marker (point-marker))
  (dbl-set-buffer)
  (send-string (get-buffer-process current-dbl-buffer)
	       (concat command "\n"))))

(defun dbl-subtitute-% (command n)
  (let* (result
	(in-dbl (get-buffer-process (current-buffer)))
	file-line 
	)
    (cond ((string-match "%[fl]" command)
	   (cond (in-dbl (setq file-line (dbl-find-line)))
		 (t (setq file-line
			  (list (buffer-file-name)
				(+ 1 (count-lines
							 (point)))))))))
    (while (and command (string-match "\\([^%]*\\)%\\([adeflp]\\)" command))
      (let ((letter (string-to-char (substring command (match-beginning 2))))
	    subst)
	(cond ((eq letter ?p)
	       (setq subst (if n (int-to-string n) "")))
	      ((eq letter ?f)
	       (setq subst (or (car file-line) "unknown-file")))
	      ((eq letter ?l)
	       (setq subst (if (cadr file-line)
			       (int-to-string (cadr file-line))
			       "unknown-line")))
	      ((eq letter ?a)
	       (setq subst (dbl-read-address))))
	(setq result (concat result
			     (substring command (match-beginning 1) (match-end 1))
			     subst)))
      (setq command (substring command (match-end 2))))
    (concat result command)))



(defun dbl-maybe-delete-prompt ()
  (if (and dbl-delete-prompt-marker
	   (> (point-max) (marker-position dbl-delete-prompt-marker)))
      (let (start)
	(goto-char dbl-delete-prompt-marker)
	(setq start (point))
	(beginning-of-line)
	(delete-region (point) start)
	(setq dbl-delete-prompt-marker nil))))

(defun dbl-break ()
  "Set DBL breakpoint at this source line."
  (interactive)
  (cond ((eq major-mode 'lisp-mode)
	 (save-excursion
	   (end-of-line)
	   (let (name
		 at where)
	     (setq where (point))
	     (mark-defun)
	     (search-forward "(def")
	     (forward-sexp 2)
	     (setq at (point))
	     (forward-sexp -1)
	     (setq name (buffer-substring (point) at))
	     (beginning-of-line)
	     (setq name (format "(si::break-function '%s %s t)"  name (count-lines 1 where)))
	     (other-window 1)
	     (if (get-buffer-process (current-buffer))
		 (setq current-dbl-buffer (current-buffer)))
	     (message name)
	     (send-string (get-buffer-process current-dbl-buffer)
			  (concat name "\n"))
	     (other-window 1)
	     )))
	(t
  
	 (let ((file-name (file-name-nondirectory buffer-file-name))
	       (line (save-restriction
		       (widen)
		       (1+ (count-lines 1 (point))))))
	   (and    downcase-filenames-for-dbl
		   (setq file-name (downcase file-name)))
	   (send-string (get-buffer-process current-dbl-buffer)
			(concat "break " file-name ":" line "\n"))))))
	
	
(defun dbl-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (dot)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(dot)))
     (cond (found (forward-char 2)(setq result
			(buffer-substring found
				 (progn (re-search-forward "[^0-9a-f]")
					(forward-char -1)
					(dot)))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (dot)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (dot)))))))


(defvar dbl-commands nil
  "List of strings or functions used by send-dbl-command.
It is for customization by you.")

(defun send-dbl-command (arg)

  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the dbl buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list dbl-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of dbl-commands.  "


  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg dbl-commands)))
    (setq addr (dbl-read-address))
    (if (eq (current-buffer) current-dbl-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-dbl-buffer)
    (goto-char (dot-max))
    (insert-string comm)))

(provide 'dbl)
