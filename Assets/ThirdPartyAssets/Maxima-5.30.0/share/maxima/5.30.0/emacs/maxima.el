;;; maxima.el --- Major modes for writing Maxima code

;; Copyright (C) 1998,1999 William F. Schelter
;; Copyright (C) 2001-2007 Jay Belanger

;; Author: William F. Schelter
;;         Jay Belanger
;;
;; Keywords: maxima

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;; You will need both maxima.el and maxima-font-lock.el

;;; Commentary:

;; This is a branch of William Schelter's maxima-mode.el
;;
;; Quick intro
;;
;; To install, put this file (as well as maxima-font-lock.el)
;; somewhere in your emacs load path.
;; To make sure that `maxima.el' is loaded when necessary, whether to
;; edit a file in maxima mode or interact with Maxima in an Emacs buffer,
;; put the lines
;;  (autoload 'maxima-mode "maxima" "Maxima mode" t)
;;  (autoload 'maxima "maxima" "Maxima interaction" t)
;; in your `.emacs' file.  If you want any file ending in `.max' to begin
;; in `maxima-mode', for example, put the line
;;  (setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))
;; to your `.emacs' file.
;;
;; In any of the Maxima modes, to get help on a prompted for Maxima topic,
;; use
;; C-c C-d h
;; or
;; f12.
;; To get help with the symbol under point, use ("d" for describe): 
;; C-c C-d d
;; or
;; C-c C-d C-d
;; For apropos help, use:
;; C-c C-d a
;; or
;; C-c C-d C-a
;; To get apropos with the symbol under point, use:
;; C-c C-d p
;; C-c C-d C-p
;; or M-f12.
;; To read the Maxima info manual, use:
;; C-c C-d m
;; C-c C-d C-m
;; C-c C-d i
;; or
;; C-c C-d C-i
;; (For Maxima minor mode, replace C-cC-d by C-c=d.)

;; ** Maxima mode **
;; To put the current buffer into maxima-mode, type M-x maxima-mode

;; Maxima mode provides the following motion commands:
;; M-C-a: Move to the beginning of the form.
;; M-C-e: Move to the end of the form.
;; M-C-b: Move to the beginning of the list.
;; M-C-f: Move to the end of the list.

;; and the following miscellaneous commands.
;; M-h: Mark the current form
;; C-c): Check the current region for balanced parentheses.
;; C-cC-): Check the current form for balanced parentheses.

;; Maxima mode has the following completions command:
;; M-TAB: Complete the Maxima symbol as much as possible, providing
;;      a completion buffer if there is more than one possible completion.

;; Portions of the buffer can be sent to a Maxima process.  (If a process is 
;; not running, one will be started.)
;; C-cC-r: Send the region to Maxima.
;; C-cC-b: Send the buffer to Maxima.
;; C-cC-c: Send the line to Maxima.
;; C-cC-e: Send the form to Maxima.
;; C-RET: Send the smallest set of lines which contains
;;        the cursor and contains no incomplete forms, and go to the next form.
;; M-RET:  As above, but with the region instead of the current line.
;; C-cC-l: Prompt for a file name to load into Maxima.
;;
;; When something is sent to Maxima, a buffer running an inferior Maxima 
;; process will appear.  It can also be made to appear by using the command
;; C-c C-p.
;; When a command is given to send information to Maxima, the region
;; (buffer, line, form) is first checked to make sure the parentheses
;; are balanced.  With an argument, they will not be checked first.
;; The Maxima process can be killed, after asking for confirmation 
;; with C-cC-k.  To kill without confirmation, give C-cC-k
;; an argument.

;; By default, indentation will be to the same level as the 
;; previous line, with an additional space added for open parentheses.
;; The behaviour of indent can be changed by the command 
;; M-x maxima-change-indent-style.  The possibilities are:
;; Standard:      Simply indent
;; Perhaps smart: Tries to guess an appropriate indentation, based on
;;                open parentheses, "do" loops, etc.
;; The default can be set by setting the value of the variable 
;; "maxima-indent-style" to either 'standard or 'perhaps-smart.
;; In both cases, M-x maxima-untab will remove a level of indentation.

;; ** Maxima noweb mode **
;; maxima-noweb-mode is a modification of maxima-mode that will work
;; nicely with noweb-mode; namely, it will limit any relevant searches 
;; to the current chunk and treat <<...>> as word parts.

;; ** Running Maxima interactively **
;; To run Maxima interactively in a buffer, type M-x maxima
;; In the Maxima process buffer,
;; return will check the line for balanced parentheses, and send line as input.
;; Control return will send the line as input without checking for balanced
;; parentheses.

;; <M-tab> will complete the Maxima symbol as much as possible, providing
;;      a completion buffer if there is more than one possible completion.

;; <C-M-tab> will complete the input line, based on previous input lines.
;; C-c C-d will get help on a Maxima topic.
;; C-c C-m will bring up the Maxima info manual.
;; C-cC-k will kill the process and the buffer, after asking for
;;   confirmation.  To kill without confirmation, give C-M-k an
;;   argument.

;; To scroll through previous commands,
;; M-p will bring the previous input to the current prompt,
;; M-n will bring the next input to the prompt.
;; M-r will bring the previous input matching
;;   a regular expression to the prompt,
;; M-s will bring the next input matching
;;   a regular expression to the prompt.

;; ** Running Maxima from the minibuffer **
;; The command M-x maxima-minibuffer
;; will allow you to interact with Maxima from the minibuffer.  
;; The arrows will allow you to scroll through previous inputs.
;; The line
;;  (autoload 'maxima-minibuffer "maxima" "Maxima in a minibuffer" t)
;; in your .emacs will make sure the function is available.
;; If the variable maxima-minibuffer-2d is non-nil, then the output
;; will be in Maxima's 2d output form, otherwise it will be in 
;; Maxima's 1d output form.  (For XEmacs, only the 1d form is available,
;; since the minibuffer isn't resizable.)
;; The command maxima-insert-last-output will insert
;; the last maxima output into the current buffer; if the output is in 2d, 
;; this will look unpleasant.  The command  maxima-insert-last-output-tex
;; will insert the TeX form of the output.

;; ** Reading Maxima results in the minibuffer **
;; The command `maxima-minibuffer-on-determined-region' 
;;   will send the part of the current buffer containing the point and between 
;;   the regexps `maxima-minor-prefix' and `maxima-minor-postfix' (currently
;;   both blank lines) to the Maxima process and insert the result in the
;;   minibuffer.  With an argument, `maxima-minibuffer-in-determined-region'
;;   will also insert the output into the current buffer, after " ==> "
;;   and before "//".  (The symbol ` ==> ' is the value of the customizable 
;;   variable `maxima-minor-output' and "//" is the value of 
;;   `maxima-minor-output-end'.  The new output is inserted, these strings 
;;   will be used to delete the old output.
;;   Outside of comments in maxima-mode, the opening and closing indicators 
;;   are the values of `maxima-mode-minor-output' and 
;;   `maxima-mode-minor-output-end', which by default are " /*==>" and 
;;   " <==*/", respectively.
;; The commands `maxima-minibuffer-on-region', `maxima-minibuffer-on-line'
;; and `maxima-minibuffer-on-form' work similarly to 
;; `maxima-minibuffer-on-determined-region', but send the current region
;; (respectively, the current line, current form) to Maxima and display
;; the result in the minibuffer.
;; (The form is the region between the preceding ; or $ and the subsequent
;; ; or $)
;; Care must be taken when inserting the output into the current buffer
;; with `maxima-minibuffer-on-region' and `maxima-minibuffer-on-form'.
;; With `maxima-minibuffer-on-region', as with 
;; `maxima-minibuffer-on-determined-region' above, everything after any
;; "==>" in the region will be ignored.  
;; What will typically happen with `maxima-minibuffer-on-region' and
;; `maxima-minibuffer-on-form', however, is that new outputs will
;; be inserted without old output being deleted.

;; The commands for the Maxima-minibuffer interaction can be made
;; available by putting 
;;  (autoload 'maxima-minibuffer "maxima" "Interact with Maxima from the minibuffer" t)
;;  (autoload 'maxima-minibuffer-on-determined-region "maxima" 
;;            "Send a information to Maxima, display the results in the minibuffer" t)
;;  (autoload 'maxima-minibuffer-on-region "maxima" 
;;            "Send a information to Maxima, display the results in the minibuffer" t)
;;  (autoload 'maxima-minibuffer-on-line "maxima" 
;;            "Send a information to Maxima, display the results in the minibuffer" t)
;;  (autoload 'maxima-minibuffer-on-form "maxima" 
;;            "Send a information to Maxima, display the results in the minibuffer" t)
;; in your .emacs

;; ** Maxima minor mode **
;; maxima-minor-mode provides convenient keybindings for the various
;; interactions between Maxima and the minibuffer.
;; It can be made easily available by placing 
;;  (autoload 'maxima-minor-mode "maxima" "Maxima minor mode" t)
;; in your .emacs, then M-x maxima-minor-mode will start the minor mode.
;; (The autoloads for the individual function will not then be necessary.)
;; C-c=e
;;   `maxima-minibuffer-on-determined-region'
;; C-c=l
;;   `maxima-minibuffer-on-line'
;; C-c=r
;;   `maxima-minibuffer-on-region'
;; C-c=f
;;   `maxima-minibuffer-on-form'
;; C-c=m
;;   `maxima-minibuffer'
;; C-c=o
;;   `maxima-insert-last-output'
;; C-c=t
;;   `maxima-insert-last-output-tex'

;;; Code:

;;;; First
(defvar maxima-running-xemacs 
  (featurep 'xemacs))

;;;; The requires

(require 'comint)
(require 'easymenu)
(require 'maxima-font-lock)

;;;; The variables that the user may wish to change

(defgroup maxima nil
  "Maxima mode"
  :prefix "maxima-"
  :tag    "Maxima")

(defcustom maxima-inchar "\\(C\\|%i\\)"
  "*The character used for an input prompt."
  :group 'maxima
  :type 'string)

(defcustom maxima-outchar "\\(D\\|%o\\)"
  "*The character used for an output prompt."
  :group 'maxima
  :type 'string)

(defcustom maxima-linechar "\\(E\\|%t\\)"
  "*The character used for an intermediate output prompt."
  :group 'maxima
  :type 'string)

(defcustom maxima-indent-amount 2
  "*The amount of each indentation level in `maxima-mode'.
This is used after `then', etc."
  :group 'maxima
  :type '(integer))

(defcustom maxima-paren-indent-amount 1
  "*The amount of indentation after a parenthesis."
  :group 'maxima
  :type '(integer))

(defcustom maxima-function-indent-amount 2
  "*The amount of extra indentation to give within functions."
  :group 'maxima
  :type 'integer)

(defvar maxima-blockparen-indent-amount nil)
(if maxima-blockparen-indent-amount
    (setq maxima-function-indent-amount
          maxima-blockparen-indent-amount))

(defcustom maxima-continuation-indent-amount 2
  "*The amount of extra indentation given when a line is continued."
  :group 'maxima
  :type '(integer))

(defcustom maxima-multiline-comment-indent-amount 2
  "*The amount of extra indentation inside a multiline comment."
  :group 'maxima
  :type '(integer))

(defcustom maxima-dont-reindent-some-comments t
  "*If non-nil, TAB will not change indentation of some comments,
namely those with nothing on the starting line past the `/*'."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-if-extra-indent-amount 0
  "*The amount of extra indentation to give a \"then\" following an \"if\"."
  :group 'maxima
  :type 'integer)

;(defcustom maxima-use-dynamic-complete nil
;  "*If non-nil, then M-TAB will complete words dynamically."
;  :group 'maxima
;  :type 'boolean)

(defcustom maxima-indent-style 'standard
  "*Determines how `maxima-mode' will handle tabs.
Choices are 'standard, 'perhaps-smart"
  :group 'maxima
  :type '(choice :menu-tag "Indent style"
                 :tag      "Indent style"
                 (const standard) 
                 (const perhaps-smart)))

(defcustom maxima-return-style 'newline-and-indent
  "*Determines how `maxima-mode' will handle RET.
Choices are 'newline, 'newline-and-indent, and 'reindent-then-newline-and-indent"
  :group 'maxima
  :type '(choice :menu-tag "Return style"
                 :tag      "Return style"
                 (const newline) 
                 (const newline-and-indent)
                 (const reindent-then-newline-and-indent)))

(defvar maxima-newline-style nil
  "For compatibility.")

(defcustom maxima-command "maxima"
  "*The command used to start Maxima. See also `maxima-args'."
  :group 'maxima
  :type 'string)

(defcustom maxima-args nil
  "*A list of extra arguments to pass to the `maxima-command'. Each
element in the list should be a distinct command-line option."
  :group 'maxima
  :type 'list)

(defcustom maxima-use-tabs nil
  "*If non-nil, indentation will use tabs."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-minibuffer-2d nil
  "*If non-nil, use 2D output for maxima-minibuffer."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-use-full-color-in-process-buffer nil
  "*If non-nil, font-lock the maxima process buffer."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-fix-double-prompt maxima-running-xemacs
  "*If non-nil, fix the double prompt that sometimes appears in XEmacs."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-save-input-history nil
  "*If non-nil, save the input history in a file."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-input-history-file "~/.maxima_history"
  "*A file to save the input history in."
  :group 'maxima
  :type 'file)

(defcustom maxima-input-history-length 50
  "*How many lines of history to save."
  :group 'maxima
  :type 'integer)

(defcustom maxima-minor-prefix "^[ \t]*$"
  "*A regexp to indicate the beginning of a region to send to Maxima
in maxima minor mode."
  :group 'maxima
  :type 'string)

(defcustom maxima-minor-postfix "^[ \t]*$"
  "*A regexp to indicate the end of a region to send to Maxima
in maxima minor mode."
  :group 'maxima
  :type 'string)

(defcustom maxima-minor-output "==>"
  "*A string to insert in the buffer right before the output."
  :group 'maxima
  :type 'string)

(defcustom maxima-minor-output-end " //"
  "*A string to insert in the buffer right after the output."
  :group 'maxima
  :type 'string)

(defcustom maxima-mode-minor-output "/*==>"
  "*A string to insert in the buffer right before the output."
  :group 'maxima
  :type 'string)

(defcustom maxima-mode-minor-output-end " <==*/"
  "*A string to insert in the buffer right after the output."
  :group 'maxima
  :type 'string)

(defcustom maxima-minor-mode-check-input t
  "*Non-nil means check the input in Maxima minor mode before sending it."
  :group 'maxima
  :type 'boolean)

(defun maxima-minor-output-mark ()
  (if (and
       (eq major-mode 'maxima-mode)
       (not (maxima-in-comment-p)))
      maxima-mode-minor-output
    maxima-minor-output))

(defun maxima-minor-output-mark-end ()
  (if (and
       (eq major-mode 'maxima-mode)
       (not (maxima-in-comment-p)))
      maxima-mode-minor-output-end
    maxima-minor-output-end))

;;;; The other variables

;; This variable seems to be necessary ...
(defvar inferior-maxima-after-output-wait 100)

(defvar maxima-temp-suffix 0
  "Temporary filename suffix.  Incremented by 1 for each filename.")

(defvar maxima-special-symbol-letters "!:='")

(defvar maxima-minibuffer-history nil)

(defvar maxima-block "")

(defvar maxima-block-wait "")

(defvar inferior-maxima-process nil
  "The Maxima process.")

(defvar inferior-maxima-input-end 0
  "The end of the latest input that was sent to Maxima.")

(defvar inferior-maxima-output-end 0)

(defvar inferior-maxima-waiting-for-output nil)

(defvar inferior-maxima-exit-hook nil)

(defvar inferior-maxima-prompt
  (concat "\\(^(" maxima-inchar 
          "[0-9]*) \\)\\|\\(^MAXIMA> \\)\\|\\(^(dbm:[0-9]*) \\)")
					; \\(^[^#%)>]*[#%)>]+ *\\)"
  "*Regexp to recognize prompts from the inferior Maxima") ; or lisp")


(defvar maxima-mode-highlight nil)
    
(defvar maxima-mode-region-begin nil)

(defvar maxima-mode-region-end nil)

(defvar maxima-minor-mode-region-begin nil)

(defvar maxima-minor-mode-region-end nil)

(defvar maxima-minor-mode-highlight nil)

(defvar maxima-minor-mode-bad-delimiter-regexp "\\([ \t\n]+\\|[0-9]+\\)")

;;;; Utility functions

(defun maxima-string-equal (str1 str2)
  (string= (downcase str1) (downcase str2)))

;; This was taken from `replace-regexp-in-string' from subr.el in GNU emacs.
(defun maxima-replace-in-string (regexp rep string)
  "Replace all matches for REGEXP with REP in STRING."
  (let ((l (length string))
	(start 0)
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	(when (= me mb) (setq me (min l (1+ mb))))
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   nil nil str)
		    (cons (substring string start mb)       ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

(defun maxima-remove-kill-buffer-hooks ()
  "Remove the kill-buffer-hooks locally"
  (if (or maxima-running-xemacs (< emacs-major-version 21))
      (progn
        (make-local-hook 'kill-buffer-hook)
        (setq kill-buffer-hook nil))
    (let ((hooks kill-buffer-hook))
      (while hooks
        (remove-hook 'kill-buffer-hook (car hooks) t)
        (setq hooks (cdr hooks))))))

(defun maxima-make-temp-name ()
  "Return a unique filename."
  (setq maxima-temp-suffix (+ maxima-temp-suffix 1))
  (concat (concat (make-temp-name "#mz") "-")
          (int-to-string maxima-temp-suffix)
          ".max"))

(defun maxima-strip-string-beginning (string)
  "Return STRING with whitespace and comments removed from the beginning."
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (out))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-remove-kill-buffer-hooks)
      (modify-syntax-entry ?/ ". 14")
      (modify-syntax-entry ?* ". 23")
      (insert string)
      (goto-char (point-min))
      (maxima-forward-over-comment-whitespace)
      (setq out (buffer-substring-no-properties (point) (point-max))))
    (kill-buffer tmpbuf)
    out))

(defun maxima-strip-string-end (string)
  "Return STRING with whitespace and comments removed from the end."
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (out))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-remove-kill-buffer-hooks)
      (modify-syntax-entry ?/ ". 14")
      (modify-syntax-entry ?* ". 23")
      (insert string)
      (goto-char (point-max))
      (maxima-back-over-comment-whitespace)
      (setq out (buffer-substring-no-properties (point-min) (point))))
    (kill-buffer tmpbuf)
    out))

(defun maxima-strip-string (string)
  "Return STRING with whitespace and comments removed from the ends."
  (maxima-strip-string-beginning (maxima-strip-string-end string)))

(defun maxima-strip-string-add-semicolon (string)
  "Return STRING with whitespace and comments removed from the ends."
  (setq string
        (maxima-strip-string-beginning (maxima-strip-string-end string)))
  (unless (or
           (string= string "")
           (and (>= (length string) 5)
                (string= (substring string 0 5) ":lisp"))
           (string= (substring string -1) ";")
           (string= (substring string -1) "$"))
    (setq string (concat string ";")))
  string)

(defun maxima-remove-whitespace-from-ends (string)
  "Return STRING with whitespace from the ends."
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (str string)
         (out)
         (beg)
         (end))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-remove-kill-buffer-hooks)
      (insert str)
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (setq beg (point))
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (setq out (buffer-substring-no-properties beg end)))
    (kill-buffer tmpbuf)
    out))

(defun maxima-remove-whitespace-from-beg (string)
  "Return STRING with whitespace removed from the beginning."
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (out)
         (str string)
         (beg)
         (end))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-remove-kill-buffer-hooks)
      (insert str)
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (setq beg (point))
      (setq out (buffer-substring-no-properties beg (point-max))))
    (kill-buffer tmpbuf)
    out))

(defun maxima-remove-whitespace-from-end (string)
  "Return STRING with whitespace removed from the end."
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (out)
         (str string)
         (beg)
         (end))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-remove-kill-buffer-hooks)
      (insert str)
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (setq out (buffer-substring-no-properties (point-min) end)))
    (kill-buffer tmpbuf)
    out))

;;;; Functions that query position
(defun maxima-in-comment-p ()
  "Non-nil means that the point is in a comment."
  (let ((pt (point)))
    (save-excursion
      (and
       (search-backward "/*" nil t)
       (not (search-forward "*/" pt t))))))

(defun maxima-in-output-p ()
  "Non-nil means that the point is in minibuffer output."
  (let ((pt (point)))
    (save-excursion
      (and
       (search-backward (maxima-minor-output-mark) nil t)
       (not (search-forward (maxima-minor-output-mark-end) pt t))))))

;;; Functions that search

;; Some additions to help with different types of searches
(defvar maxima-mode-type 'maxima-mode)
(make-variable-buffer-local 'maxima-mode-type)

(defvar maxima-noweb-ignore-bounds '("<<" ">>"))

(defun maxima-noweb-in-ignore-bounds-p ()
  (if (not maxima-noweb-ignore-bounds)
      nil
    (let ((pt (point)))
      (save-excursion
        (if (not (re-search-backward (car maxima-noweb-ignore-bounds) nil t))
            nil
          (not (re-search-forward (cadr maxima-noweb-ignore-bounds) pt t)))))))

(defun maxima-noweb-forward-out-of-ignore-bounds (&optional pmax)
  (re-search-forward (cadr maxima-noweb-ignore-bounds) pmax 1))

(defun maxima-noweb-backward-out-of-ignore-bounds (&optional pmin)
  (re-search-backward (car maxima-noweb-ignore-bounds) pmin 1))


(defun maxima-standard-re-search-forward (regexp &optional pmax)
  "Search forward for REGEXP, bounded by PMAX.
Ignore matches found in comments and strings."
  (let ((keep-looking t)
        (didnt-find nil)
        (match nil)
        (ppe)
        (pt)
        (origpt (point)))
    (setq pt origpt)
    (while (and keep-looking
                (not didnt-find)
                (re-search-forward regexp pmax t))
      (setq match (match-string 0))
      (setq ppe (parse-partial-sexp pt (point)))
      (cond
       ((nth 3 ppe)  ;; In a string
        (if (maxima-goto-end-of-string)
            (setq pt (point))
          (setq didnt-find t)))
       ((nth 4 ppe)  ;; In a comment
        (if (maxima-goto-end-of-comment)
            (setq pt (point))
          (setq didnt-find t)))
       (t            ;; not in a comment or string
        (setq keep-looking nil))))
    (if (or didnt-find keep-looking)
        (progn
          (goto-char origpt)
          nil)
      match)))


(defun maxima-noweb-re-search-forward (regexp &optional pmax)
  (let ((match
         (maxima-standard-re-search-forward regexp pmax)))
    (while (maxima-noweb-in-ignore-bounds-p)
      (maxima-noweb-forward-out-of-ignore-bounds pmax)
      (setq match
            (maxima-standard-re-search-forward regexp pmax)))
    match))

(defun maxima-re-search-forward (regexp &optional pmax)
  (cond
   ((eq maxima-mode-type 'maxima-noweb-mode)
    (maxima-noweb-re-search-forward regexp pmax))
   (t
    (maxima-standard-re-search-forward regexp pmax))))

(defun maxima-re-search-forward-skip-blocks (regexp &optional pmax)
  "Search forward for REGEXP, bounded by PMAX.
Ignore matches found in comments and strings, and skip over
parenthesized or bracketed blocks."
  (let ((keep-looking t)
        (didnt-find nil)
        (match nil)
        (pt (point)))
    (while (and keep-looking 
                (setq match (maxima-re-search-forward
                              (concat regexp "\\|[[(]") pmax)))
      (cond
       ((or 
         (string= match "[")
         (string= match "("))
        (unless
            (maxima-goto-end-of-list)
          (setq didnt-find t)
          (setq keep-looking nil)))
       (t
        (setq keep-looking nil))))
    (if (or keep-looking didnt-find)
        (progn
          (goto-char pt)
          nil)
      match)))

(defun maxima-standard-re-search-backward (regexp &optional pmin)
  "Search backward for REGEXP, bounded by PMIN.
Ignore matches found in comments and strings."
  (let ((keep-looking t)
        (match nil)
        (ppe)
        (origpt (point)))
    (unless pmin 
      (setq pmin (point-min)))
    (while (and keep-looking
                (re-search-backward regexp pmin t))
      (setq match (match-string 0))
      (setq ppe (parse-partial-sexp pmin (point)))
      (cond
       ((nth 8 ppe)  ;; In a string or comment
        (goto-char (nth 8 ppe)))
       (t            ;; not in a comment or string
        (setq keep-looking nil))))
    (if keep-looking
        (progn
          (goto-char origpt)
          nil)
      match)))

(defun maxima-noweb-re-search-backward (regexp &optional pmin)
  (let ((match
         (maxima-standard-re-search-backward regexp pmin)))
    (while (maxima-noweb-in-ignore-bounds-p)
      (maxima-noweb-backward-out-of-ignore-bounds pmin)
      (setq match
            (maxima-standard-re-search-backward regexp pmin)))
    match))

(defun maxima-re-search-backward (regexp &optional pmin)
  (cond
   ((eq maxima-mode-type 'maxima-noweb-mode)
    (maxima-noweb-re-search-backward regexp pmin))
   (t
    (maxima-standard-re-search-backward regexp pmin))))

(defun maxima-re-search-backward-skip-blocks (regexp &optional pmin)
  "Search forward for REGEXP, bounded by PMIN.
Ignore matches found in comments and strings, and skip over
parenthesized and bracketed blocks."
  (let ((keep-looking t)
        (didnt-find nil)
        (match nil)
        (pt (point)))
    (while (and keep-looking 
                (setq match (maxima-re-search-backward
                              (concat regexp "\\|[])]") pmin)))
      (cond
       ((or 
         (string= match ")")
         (string= match "]"))
        (unless
            (maxima-goto-beginning-of-list)
          (setq didnt-find t)
          (setq keep-looking nil)))
       (t
        (setq keep-looking nil))))
    (if (or keep-looking didnt-find)
        (progn
          (goto-char pt)
          nil)
      match)))

(defun maxima-escaped-char-p ()
  "Returns non-nil if the character after point is escaped"
  (let ((pm (point-min))
        (esc-chars 0))
    (when (> (point) pm)
      (save-excursion
        (forward-char -1)
        (while (and
                (looking-at "\\\\")
                (setq esc-chars (1+ esc-chars))
                (> (point) pm))
          (forward-char -1))))
    (if (= (% esc-chars 2) 0)
        nil
      t)))

(defun maxima-goto-end-of-string ()
  "Go to the end of the string that the point is in.
Assumes that point is in a string."
  (interactive)
  (let ((keep-looking t))
    (while (and keep-looking (search-forward "\"" nil t))
      (forward-char -2)
      (unless (maxima-escaped-char-p)
        (setq keep-looking nil))
      (forward-char 2))
    (if keep-looking
        nil
      t)))
        
(defun maxima-goto-beginning-of-string ()
  "Go to the beginning of the string that the point is in.
Assumes that point is in a string."
  (interactive)
  (let ((keep-looking t))
    (while (and keep-looking (search-backward "\"" nil t))
      (forward-char -1)
      (unless (maxima-escaped-char-p)
        (setq keep-looking nil))
      (forward-char 1))))

(defun maxima-goto-end-of-comment ()
  "Go to the end of the comment that the point is in.
Assumes that point is in a comment."
  (interactive)
  (search-forward "*/" nil t))
        
(defun maxima-goto-beginning-of-comment ()
  "Go to the beginning of the comment that the point is in.
Assumes that point is in a comment."
  (interactive)
  (search-backward "/*"))
        
(defun maxima-find-next-nonnested-close-char ()
  "Search forward for next , ; $ or closing ), skipping over balanced parens.
If character is in a string or a list, ignore it."
  (interactive)
  (maxima-re-search-forward-skip-blocks "[,;$)]"))


;;; Functions for dealing with words

;; (defun maxima-number-of-preceding-backslashes ()
;;   "The number of backslashes (including the one being looked at)."
;;   (let ((pt (point)))
;;     (if (not (looking-at "\\\\"))
;;         0
;;       (save-excursion
;;         (skip-chars-backward "\\\\")
;;         (1+ (- pt (point)))))))

(defun maxima-standard-next-char-word-part-p ()
  "Non-nil if next char is a a word part."
  (or
   (looking-at "\\w")
   (looking-at "\\\\")
   (save-excursion
     (forward-char -1)
     (looking-at "\\\\"))))

(defun maxima-noweb-next-char-word-part-p ()
  "Non-nil if next char is a a word part."
  (or
   (looking-at "\\w")
   (looking-at "\\\\")
   (and
    (looking-at ">")
    (save-excursion
      (forward-char -1)
      (looking-at ">")))
   (save-excursion
     (forward-char -1)
     (looking-at "\\\\"))))

(defun maxima-next-char-word-part-p ()
  (cond
   ((eq maxima-mode-type 'maxima-noweb-mode)
    (maxima-noweb-next-char-word-part-p))
   (t
    (maxima-standard-next-char-word-part-p))))

(defun maxima-previous-char-word-part-p ()
  "Non-nil if previous character is a word part."
  (save-excursion
    (forward-char -1)
    (maxima-next-char-word-part-p)))

(defun maxima-standard-forward-word ()
  "Go to the end of the current word."
  (let ((keep-going t))
    (while keep-going
      (cond
       ((looking-at "\\w")
        (forward-word 1))
       ((looking-at "\\\\")
        (forward-char 2))
       (t
        (setq keep-going nil))))))

(defun maxima-noweb-forward-word ()
  "Go to the end of the current word."
  (if (maxima-noweb-in-ignore-bounds-p)
      (maxima-noweb-forward-out-of-ignore-bounds))
  (let ((keep-going t))
    (while keep-going
      (cond
       ((looking-at "\\w")
        (forward-word 1))
       ((looking-at "\\\\")
        (forward-char 2))
       ((looking-at "<<")
        (forward-char 2)
        (maxima-noweb-forward-out-of-ignore-bounds))
       (t
        (setq keep-going nil))))))

(defun maxima-forward-word ()
  (cond
   ((eq maxima-mode-type 'maxima-noweb-mode)
    (maxima-noweb-forward-word))
   (t
    (maxima-standard-forward-word))))

(defun maxima-standard-backward-word ()
  "Go to the beginning of the current word."
  (let ((keep-going t))
    (while keep-going
      (cond
       ((and
         (> (point) (point-min))
         (save-excursion
           (forward-char -1)
           (looking-at "\\w")))
        (backward-word 1))
       ((and
         (> (point) (1+ (point-min)))
         (save-excursion
           (forward-char -2)
           (looking-at "\\\\")))
        (forward-char -2))
       (t
        (setq keep-going nil))))))

(defun maxima-noweb-backward-word ()
  "Go to the beginning of the current word."
  (if (maxima-noweb-in-ignore-bounds-p)
      (maxima-noweb-backward-out-of-ignore-bounds))
  (let ((keep-going t))
    (while keep-going
      (cond
       ((and
         (> (point) (point-min))
         (save-excursion
           (forward-char -1)
           (looking-at "\\w")))
        (backward-word 1))
       ((and
         (> (point) (1+ (point-min)))
         (save-excursion
           (forward-char -2)
           (looking-at "\\\\")))
        (forward-char -2))
       ((and
         (> (point) (1+ (point-min)))
         (save-excursion
           (forward-char -2)
           (looking-at ">>")))
        (forward-char -2)
        (maxima-noweb-backward-out-of-ignore-bounds))
       (t
        (setq keep-going nil))))))

(defun maxima-backward-word ()
  (cond
   ((eq maxima-mode-type 'maxima-noweb-mode)
    (maxima-noweb-backward-word))
   (t
    (maxima-standard-backward-word))))

;;;; Functions that return special positions

(defun maxima-line-beginning-position ()
  (if (not (fboundp 'line-beginning-position))
      (save-excursion
	(beginning-of-line)
	(point))
    (line-beginning-position)))

(defun maxima-line-end-position ()
  (if (not (fboundp 'line-end-position))
      (save-excursion
	(end-of-line)
	(point))
    (line-end-position)))

(defun maxima-name-beginning ()
  (save-excursion
    (maxima-backward-word)
    (point)))

(defun maxima-special-symbol-beginning ()
  (save-excursion
    (skip-chars-backward maxima-special-symbol-letters)
    (point)))

(defun maxima-special-symbol-end ()
  (save-excursion
    (skip-chars-forward maxima-special-symbol-letters)
    (point)))

(defun maxima-form-beginning-position ()
  (save-excursion
    (maxima-goto-beginning-of-form)
    (point)))

(defun maxima-form-end-position ()
  (save-excursion
    (if (maxima-goto-end-of-form)
        (point)
      nil)))

(defun maxima-form-end-position-or-point-max ()
  (let ((mfep (maxima-form-end-position)))
    (if mfep
        mfep
      (point-max))))

(defun maxima-expression-end-position ()
  "Return the point where the current expression ends,
or nil."
  (save-excursion
    (if (maxima-goto-end-of-expression)
        (point)
      nil)))

(defun maxima-begin-if-position (pmin)
  "Find the point of the opening \"if\" for the current point."
  (let ((nest 0)
        (match)
        (keep-looking t)
        (pt (point)))
    (save-excursion
      (while (and keep-looking 
                  (setq match 
                        (maxima-re-search-backward-skip-blocks 
                         "\\<if\\>\\|\\<then\\>" pmin)))
        (setq match (downcase match))
        (cond ((maxima-string-equal match "if") (setq nest (1+ nest)))
              (t (setq nest (1- nest))))
        (when (= nest 1)
          (setq pt (point))
          (setq keep-looking nil))))
    (if keep-looking
        nil
      pt)))

(defun maxima-begin-then-position (pmin)
  "Find the point of the opening \"then\" for the current \"else\"."
  (let ((keep-going t)
        (pt (point))
        (begin-then nil))
  (save-excursion
    (while (and keep-going
                (maxima-re-search-backward-skip-blocks "\\<then\\>" pmin))
      ;; A potential "then".  Let's see if it is.
      (let ((meep (maxima-expression-end-position)))
        (when (or (not meep) (<= pt meep))
          ;; This "then" is looking pretty good.
          ;; Now we need to make sure that there aren't any "else"s
          ;; in the way.
          (let ((level 0)
                (match))
            (save-excursion
              (while (setq match (maxima-re-search-forward-skip-blocks 
                                  "\\<then\\>\\|\\<else\\>" pt))
                (cond ((maxima-string-equal match "then")
                       (setq level (1+ level)))
                      ((maxima-string-equal match "else")
                       (setq level (1- level))))))
            (when (= level 1)
              (setq begin-then (point))
              (setq keep-going nil))))))
      begin-then)))

;;;; Functions that move the position
(defun maxima-forward-over-comment-whitespace ()
  "Move forward over comments and whitespace."
  (forward-comment (buffer-size))
  (let ((mmo (maxima-remove-whitespace-from-beg (maxima-minor-output-mark))))
    (when (and (> (- (point-max) (point)) (length mmo))
               (string= 
                (buffer-substring-no-properties 
                 (point) (+ (point) (length mmo)))
                mmo))
      (search-forward (maxima-minor-output-mark-end))
      (forward-comment (buffer-size)))))

(defun maxima-back-over-comment-whitespace ()
  "Move backward over comments and whitespace."
  (forward-comment (- (buffer-size)))
  (let ((mme (maxima-remove-whitespace-from-end (maxima-minor-output-mark-end))))
    (when (and (> (- (point) (point-min)) (length mme))
               (string= 
                (buffer-substring-no-properties 
                 (- (point) (length mme)) (point))
                mme))
      (search-backward (maxima-minor-output-mark))
      (forward-comment (- (buffer-size))))))

(defun maxima-standard-goto-beginning-of-form ()
  "Move to the beginning of the form."
  (let ((pt (point))
        (keep-looking t))
    (while (and keep-looking
                (maxima-re-search-backward "[;$]" nil))
      (forward-char -1)
      (unless (looking-at "\\\\\\$")
        (forward-char 2)
        (setq keep-looking nil)))
    (if keep-looking
        (goto-char (point-min)))
    (maxima-forward-over-comment-whitespace)
    (if (< pt (point))
        (goto-char pt))
    (point)))

(defun maxima-noweb-goto-beginning-of-form ()
  "Move to the beginning of the form."
  (if (re-search-backward "^<<.*?>>= *$" (point-min) 1)
      (forward-line 1))
  (maxima-forward-over-comment-whitespace))

(defun maxima-goto-beginning-of-form ()
  (cond
   ((eq maxima-mode-type 'maxima-noweb-mode)
    (maxima-noweb-goto-beginning-of-form))
   (t
    (maxima-standard-goto-beginning-of-form))))

(defun maxima-goto-beginning-of-form-interactive ()
  "Move to the beginning of the form."
  (interactive)
  (maxima-goto-beginning-of-form))

(defun maxima-standard-goto-end-of-form ()
  "Move to the end of the form."
  (let ((keep-looking t)
        (pt (point)))
    (while (and keep-looking 
                (maxima-re-search-forward "[;$]" nil))
      (forward-char -2)
      (unless (looking-at "\\\\\\$")
        (setq keep-looking nil))
      (forward-char 2))
    (if (not keep-looking)
        (point)
      (goto-char pt)
      nil)))

(defun maxima-noweb-goto-end-of-form ()
  "Move to the end of the form."
  (when (re-search-forward "\\(^@\\( \\|$\\)\\|^<<.*>>= *$\\)" nil 1)
    (forward-line -1)
    (end-of-line)
    (maxima-back-over-comment-whitespace)))

(defun maxima-goto-end-of-form ()
  (cond
   ((eq maxima-mode-type 'maxima-noweb-mode)
    (maxima-noweb-goto-end-of-form))
   (t
    (maxima-standard-goto-end-of-form))))

(defun maxima-goto-end-of-form-interactive ()
  "Move to the end of the form."
  (interactive)
  (unless (maxima-goto-end-of-form)
    (message "No end of form")))

(defun maxima-goto-end-of-expression ()
  "Find the point that ends the expression that begins at point.
The expression is assumed to begin with \"if\", \"then\", \"do\"
\"else\" or \"(\".  Return nil if the end is not found."
;  (interactive)
  ;; To find the end of the expression:
  ;;  if looking at (, look for )
  ;;  otherwise look for a , ; or $ at the same nesting level of
  ;;  parentheses or a closing ).
  (cond ((or (looking-at "(") 
             (looking-at "\\["))
         (maxima-forward-list))
        (t
         (maxima-find-next-nonnested-close-char))))

(defun maxima-goto-beginning-of-construct (pmin)
  "Go to the point the begins the current construct."
 (let ((keep-looking t)
        (pt (point)))
    (while (and keep-looking 
                (maxima-re-search-backward-skip-blocks 
                 "\\<if\\>\\|\\<then\\>\\|\\<do\\>\\|\\<else\\>\\|(\\|\\[" pmin))
      (save-excursion
        (when (or (not (maxima-goto-end-of-expression)) (<= pt (point)))
          (setq keep-looking nil))))
    (if keep-looking 
        (goto-char pmin))
    (point)))

(defun maxima-goto-end-of-list ()
  "Go up a list.
Return t if possible, nil otherwise."
  (interactive)
  (if
      (condition-case nil
          (up-list 1)
        (error t))
      nil
    t))

(defun maxima-goto-end-of-list-interactive ()
  "Go up a list."
  (interactive)
  (if (maxima-goto-end-of-list)
      t
    (error "No list to end.")))

(defun maxima-goto-beginning-of-list ()
  "Go up a list backwards.
Return t if possible, nil otherwise."
  (if
      (condition-case nil
          (up-list -1)
        (error t))
      nil
    t))

(defun maxima-goto-beginning-of-list-interactive ()
  "Go up a list."
  (interactive)
  (if (maxima-goto-beginning-of-list)
      t
    (error "No list to begin.")))

(defun maxima-forward-list ()
  "Go forward a list.
Return t if possible, nil otherwise."
  (if
      (condition-case nil
          (forward-list 1)
        (error nil))
      t
    nil))

(defun maxima-backward-list ()
  "Go backward a list.
Return t if possible, nil otherwise."
  (if
      (condition-case nil
          (backward-list 1)
        (error nil))
      t
    nil))

;;; Newlines and indents
(defun maxima-indent-form ()
  "Indent the entire form."
  (interactive)
  (indent-region
   (maxima-form-beginning-position)
   (maxima-form-end-position-or-point-max)
   nil))

;;; 'standard
(defun maxima-standard-indent ()
  "Indent the line based on the previous line.
If the previous line opened a parenthesis, `maxima-indent-amount' is
added to the indentation, if the previous line closed a parenthesis, 
`maxima-indent-amount' is subtracted, otherwise the indentation 
is the same as the previous line."
  (interactive)
  (let ((indent 0)
        (match)
        (pt))
    (save-excursion
      (when (= (forward-line -1) 0)
        (progn
          (setq indent (current-indentation))
          (setq pt (maxima-line-end-position))
          (while (setq match (maxima-re-search-forward "[()]" pt))
            (cond ((string= match ")")
                   (setq indent (- indent maxima-indent-amount)))
                  ((string= match "(")
                   (setq indent (+ indent maxima-indent-amount))))))))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-line-to (max indent 0)))
    (skip-chars-forward " \t")))

(defun maxima-untab ()
  "Delete a level of indentation"
  (interactive)
  ;; Check to see if the previous maxima-indent-amount spaces are 
  ;; on the same line and blank
  (let ((i maxima-indent-amount)
	(ok t)
	(pt (point)))
    (save-excursion
      (while (and (> i 0) ok)
	(setq i (- i 1))
	(forward-char -1)
	(if (not (looking-at " "))
	    (setq ok nil))
	(if (looking-at "\n")
	    (setq ok nil))))
    (if ok
	(delete-region pt (- pt maxima-indent-amount)))))


;;; Find the beginning of a function

(defun maxima-open-paren-is-function ()
  "Check to see if the point is before an opening paren,
and if the previous character is a close paren or word part."
  (and
   (looking-at "[ \t]*(")
   (save-excursion
     (maxima-back-over-comment-whitespace)
     (forward-char -1)
     (looking-at ")\\|\\w"))))

(defun maxima-close-paren-before-open-paren ()
  "Return point of close paren before the current open paren,
or nil if there is none."
  (save-excursion
    (maxima-back-over-comment-whitespace)
    (forward-char -1)
    (if (looking-at ")")
        (1+ (point))
      nil)))

(defun maxima-back-over-paren-groups ()
  "Go over any paren groups.
Assume point is at \"(\", as long as preceding character is
\")\", go to open parentheses."
  (let ((cpbop (maxima-close-paren-before-open-paren)))
    (while cpbop
      (goto-char cpbop)
      (maxima-backward-list)
      (setq cpbop
            (maxima-close-paren-before-open-paren)))))

(defun maxima-word-on-previous-line ()
  "Go to the previous word part, return nil if "
  (let ((pt (point)))
    (save-excursion
      (skip-chars-backward " \t\n")
      ;; Same line?
      (and 
       (maxima-previous-char-word-part-p)
       (string-match "\n"
                     (buffer-substring-no-properties (point) pt))))))

(defun maxima-string-on-previous-line ()
  "Go to the previous word part, return nil if "
  (let ((pt (point)))
    (save-excursion
      (skip-chars-backward " \t\n")
      ;; Same line?
      (forward-char -1)
      (and 
       (looking-at "\"")
       (string-match "\n"
                     (buffer-substring-no-properties (point) pt))))))

(defun maxima-back-over-function-name ()
  "Go back over the `foo' in 'foo(x,y)', return the length of `foo'.
Return nil if open paren is not part of function.
Assumes the point is right before the open parenthesis."
  (let ((pt (point))
        (endpt))
    (maxima-back-over-paren-groups)
    (setq endpt (point))
    ;; Let's see what's before this
    (cond
     ;; There is a word right before this
     ((maxima-previous-char-word-part-p)
      (maxima-backward-word)
      (- endpt (point)))
     ;; There is a string before this
     ((save-excursion
        (forward-char -1)
        (looking-at "\""))
      (maxima-goto-beginning-of-string)
      (- endpt (point)))
     ;; There is a word before this on the previous line
     ((maxima-word-on-previous-line)
      (skip-chars-backward " \t\n")
      (setq endpt (point))
      (maxima-backward-word)
      (- endpt (point)))
     ;; There is a string before this on the previous line
     ((maxima-string-on-previous-line)
      (skip-chars-backward " \t\n")
      (setq endpt (point))
      (maxima-goto-beginning-of-string)
      (- endpt (point)))
     ;; This point is the original point
     ((= endpt pt)
      nil)
     ;; Finally, the last parenthesized expression is the function
     (t
      (save-excursion
        (maxima-forward-list)
        (setq endpt (point)))
      (- endpt (point))))))

;;; 'perhaps-smart

(defun maxima-after-lisp-expression-p ()
  "Return non-nil if the point is right after a lisp expression."
  (let ((pm (point-min))
        (pt))
    (save-excursion
      (maxima-back-over-comment-whitespace)
      (setq pt (point))
      (condition-case nil
          (backward-sexp)
        (error t))
      (when (< (point) pt)
        (maxima-back-over-comment-whitespace)
        (if (< (point) (+ 5 pm))
            nil
          (forward-char -5)
          (if (looking-at ":lisp")
              (current-column)
            nil))))))

(defun maxima-standard-perhaps-smart-calculate-indent ()
  "Return appropriate indentation for current line as Maxima code.
Returns an integer: the column to indent to."
  (let ((indent 0)
        (pmin)
        (tmpchar)
        (pt)
        (le)
        (comma-line)
        (len)
        (pps))
    (save-excursion
      (beginning-of-line)
      (setq pt (point))
      (setq pmin (maxima-form-beginning-position))
      (setq pps (parse-partial-sexp pmin (point)))
      (setq le (maxima-after-lisp-expression-p))
      (when (nth 1 pps)
        (setq pmin (nth 1 pps))
        (unless (looking-at "^[ \t]*,")
          (save-excursion
            (when (maxima-re-search-backward-skip-blocks "," pmin)
              (save-excursion
                (let ((lep (maxima-line-end-position)))
                  (forward-char 1)
                  (maxima-forward-over-comment-whitespace)
                  (unless (>= (point) lep)
                    (setq pmin (point)))))))))
      (cond
       ;; First, take care of the cases where the indentation is clear
       ;; No indentation at the beginning of the buffer
       ((= pt (point-min))
        (setq indent 0))
       ;; Don't change the indentation if in a string
       ((nth 3 pps)
        (setq indent -1))
       ;; If the line begins a comment and comments aren't reindented,
       ;; don't reindent it
       ((and
         (looking-at "[ \t]*/\\*[ \t]*$")
         maxima-dont-reindent-some-comments)
        (setq indent -1))
       ;; Deal with comments separately
       ((maxima-perhaps-smart-in-comment-p (nth 4 pps) pmin pt)
        (setq indent (maxima-perhaps-smart-comment-calculate-indent)))
       ;; If the current point is the beginning of the form, the level is 0
       ((= (point) pmin)
        (setq indent 0))
       ;; If the current point is in maxima minor output, don't reindent it
       ((maxima-in-output-p)
        (setq indent -1))
       ;; A line beginning "then" is indented like the opening "if"
       ((and
         (looking-at "[ \t]*\\<then\\>")
         (setq tmpchar (maxima-begin-if-position pmin)))
        (goto-char tmpchar)
        (setq indent (+ maxima-if-extra-indent-amount (current-column))))
       ;; A line beginning "else" is indented like the corresponding "then"
       ((and
         (looking-at "[ \t]*\\<else\\>")
         (setq tmpchar (maxima-begin-then-position pmin)))
        (goto-char tmpchar)
        (setq indent (current-column)))
       ;; A line beginning with an open paren that is the
       ;; beginning of a function argument is indented according to
       ;; the function
       ((and
         (looking-at "[ \t]*(")
         (setq len (maxima-back-over-function-name)))
        (setq indent (+ (current-column) 
                        (min len maxima-function-indent-amount))))
       ;; A line beginning with a closing paren is indented like the open paren
       ((looking-at "[ \t]*)")
        ;; Here, pmin should be the opening paren position
        (goto-char pmin)
        (if (looking-at "( *$")
            (progn
              (setq len (maxima-back-over-function-name))
              (setq indent (+ 
                            (if len
                                (min len maxima-function-indent-amount)
                              0)
                            (current-column))))
          (setq indent (current-column))))
       ;; A line beginning with a closing bracket is indented like the open bracket
       ((looking-at "[ \t]*\\]")
        ;; Here, pmin should be the opening bracket position
        (goto-char pmin)
        (setq indent (current-column)))
;       ;; A line beginning with a comma is indented like the opening paren
;       ((looking-at "[ \t]*,")
;        (goto-char pmin)
;        (setq indent (current-column)))
       ;; The point is at the end of a lisp expression
       (le
        (setq indent le))
       ;; Otherwise, the correct indentation needs to be computed.
       (t 
        ;; Find the indentation of beginning of the current construct
        ;; If begin-construct is nil, indent according to the opening paren
        (setq comma-line (looking-at "[ \t]*,"))
        (save-excursion
          (maxima-goto-beginning-of-construct pmin)
          (cond
           ;; The construct begins with a bracket
           ((looking-at "\\[")
;            (if comma-line
;                (setq indent (current-column))
              (setq indent (+ maxima-paren-indent-amount (current-column)))
              (forward-char 1)
              (skip-chars-forward " \t")
              (unless (looking-at "\n")
                (setq indent (current-column))));)
           ;; The construct begins with a paren
           ((looking-at "(")
            (cond
;             (comma-line
;              (setq indent (current-column)))
             ((save-excursion
                (let ((lep (maxima-line-end-position)))
                  (forward-char 1)
                  (maxima-forward-over-comment-whitespace)
                  (>= (point) lep)))
              ;(looking-at "( *$")
              ;; Check to see if there is anything before the (
              (if (save-excursion
                    (re-search-backward "\\(?:^[ \t]*\\)\\=" nil t))
                  (setq tmpchar maxima-paren-indent-amount)
                (setq tmpchar 0))
              ;; If there is nothing after the (, there are two
              ;; cases
              ;; First, there is a function before it
              (if (setq len (maxima-back-over-function-name))
                  (setq indent (+ (min len maxima-function-indent-amount)
                                  tmpchar
                                  (current-column)))
                ;; Otherwise,
                (setq indent (+ maxima-paren-indent-amount (current-column)))))
             ;; If there is something after the (, indent according to that
             (t
              (forward-char 1)
              (skip-chars-forward " \t")
              (setq indent (current-column)))))
           ;; The construct does not begin with a paren
           (t
              (setq indent (current-column)))))
        ;; Now, we need to possibly do some adjustments.
        ;; If the previous column doesn't end with close-char or a 
        ;; parenthesis, assume that the current line in a continuation 
        ;; of that line, and add to the indentation, unless the 
        ;; previous line was the beginning of the construct containing 
        ;; the point or only an open parenthesis.
        (if comma-line
            (let ((bol-pt (point))
                  comma-pt
                  diff)
              (skip-chars-forward " \t")
              (setq comma-pt (point))
              (forward-char 1)
              (skip-chars-forward " \t")
              (setq diff (- (point) comma-pt))
              (if (> diff indent)
                  (let ((lineno
                         (save-excursion
                           (forward-line 0)
                           (1+ (count-lines (point-min) (point))))))
                    (message 
                     (format "Leading comma prevents proper indentation on line %d"
                             lineno))
                    (setq indent 0))
                (setq indent (- indent diff))))
          (maxima-back-over-comment-whitespace)
          (when (not (looking-at "^"))
            (forward-char -1)
            (if (not (or le (looking-at "[,;$]")))
                (setq indent (+ maxima-continuation-indent-amount indent))))))))
    indent))

(defun maxima-noweb-perhaps-smart-calculate-indent ()
  (let ((indent nil)
        (pt))
    (save-excursion
      (beginning-of-line)
      (cond
       ((looking-at "^<<.*?>>=[ \t]*$")
        (setq indent -1))
       ((looking-at "^@[ \n]")
        (setq indent -1))
       (t
        (forward-line -1)
        (if (looking-at "^<<.*?>>=[ \t]*$")
            (setq indent -1)))))
    (if indent
        indent
      (maxima-standard-perhaps-smart-calculate-indent))))

(defun maxima-perhaps-smart-calculate-indent ()
  (cond
   ((eq maxima-mode-type 'maxima-mode)
    (maxima-standard-perhaps-smart-calculate-indent))
   ((eq maxima-mode-type 'maxima-noweb-mode)
    (maxima-noweb-perhaps-smart-calculate-indent))))


(defun maxima-perhaps-smart-in-comment-p (incomment pmin pt)
  "Determine if the point is in a comment or not."
  (cond 
   ;; If we're told it's in a comment, then it is.
   (incomment
    t)
   ;; Otherwise, if pmin is less than point, we're not in a comment
   ((> pt pmin)
    nil)
   ;; Otherwise, we have to check it out
   (t
    (maxima-in-comment-p))))

(defun maxima-perhaps-smart-comment-calculate-indent ()
  "Calculate the indentation of the current line,
which is in a comment which begins on a previous line."
  (let ((indent 0)
        (blankline nil)
        (endcomment nil))
    (cond
     ((looking-at "^[ \t]*$")
        (setq blankline t))
     ((looking-at "^[ \t]*\\*/")
        (setq endcomment t)))
    ;; First of all, if the comment begins with `/*' and nothing
    ;; else on the line, don't indent it.
    (save-excursion
      (maxima-goto-beginning-of-comment)
      (cond
      ;; Take care of the case when comments won't be indented
       ((and
         (looking-at "/\\*[ \t]*$")
         maxima-dont-reindent-some-comments)
        (if blankline
            (setq indent (+ (current-column) 
                            maxima-multiline-comment-indent-amount))
          (setq indent -1)))
       ;; Now, the other cases
       ;; If the current line ends a column, indent it like the opening line
       (endcomment
        (setq indent (current-column)))
       ;; If the opening line has a blank after the `/*'
       ((looking-at "/\\*[ \t\n]")
        (forward-char 2)
        (skip-chars-forward " \t")
        (if (not (looking-at "\n"))
            (setq indent (current-column))
          (search-backward "/*")
          (setq indent (+ (current-column) 
                          maxima-multiline-comment-indent-amount))))
       (t
        (setq indent (+ (current-column) 
                        maxima-multiline-comment-indent-amount)))))
    indent))

(defun maxima-perhaps-smart-indent-line ()
  "Reindent the current line."
  (interactive)
  (let ((indent (maxima-perhaps-smart-calculate-indent)))
    (unless (= indent -1)
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to indent)))
    (skip-chars-forward " \t")))

;;;; Indentation according to style

(defun maxima-indent-line ()
  (interactive)
  (cond
   ((eq maxima-newline-style 'basic)
    (maxima-standard-indent))
   ((eq maxima-indent-style 'standard)
    (maxima-standard-indent))
   ((eq maxima-indent-style 'perhaps-smart)
    (maxima-perhaps-smart-indent-line))))

(defun maxima-change-indent-style (new-style)
  "Change the newline style."
  (interactive "sNewline style (insert \"b\" for basic, \"s\" for standard, or \"p\" for perhaps-smart): ")
  (cond
   ((string= new-style "b")
    (setq maxima-indent-style 'basic))
   ((string= new-style "s")
    (setq maxima-indent-style 'standard))
   ((string= new-style "p")
    (setq maxima-indent-style 'perhaps-smart))))

(defun maxima-return ()
  (interactive)
  (cond
   ((eq maxima-return-style 'newline)
    (newline))
   ((eq maxima-return-style 'newline-and-indent)
    (newline-and-indent))
   ((eq maxima-return-style 'reindent-then-newline-and-indent)
    (reindent-then-newline-and-indent))))

;;;; Commenting

(defun maxima-insert-short-comment ()
  "Prompt for a comment."
  (interactive)
  (let ((comment (read-string "Comment: ")))
    (insert "/* " comment " */")
    (newline-and-indent)))

(defun maxima-insert-long-comment ()
  "Insert a comment enviroment"
  (interactive)
  (indent-for-tab-command)
  (insert "/*")
  (newline)
  (newline)
  (insert "*/")
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command))

(defun maxima-uncomment-region (beg end)
  "`uncomment-region' to use with the menu."
  (interactive "r")
  (uncomment-region beg end (universal-argument)))

;;;; Help functions

(defun maxima-goto-info-node (node)
  (if maxima-running-xemacs
      (info "Maxima")
    (info-other-window (concat "(Maxima)" node))))

(defun maxima-get-info-on-subject (subject &optional same-window)
  (info "Maxima")
  (Info-menu "Function and Variable Index")
  (cond (maxima-running-xemacs
	 (or (and (search-forward subject nil t)
		  (Info-follow-nearest-node (point)))
	     (message (concat "Unable to locate " subject))))
	(t
	 (Info-menu subject))))

(defun maxima-get-help ()
  "Get help on a given subject"
  (let ((pt)
        (place))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward "* ")
      (setq pt (point))
      (search-forward ":")
      (skip-chars-backward ": ")
      (setq name (buffer-substring-no-properties pt (point))))
    (maxima-get-info-on-subject name t)))
;    (if (not maxima-running-xemacs)
;        (info-other-window (concat "(Maxima)" place))
;      (info "Maxima")
;      (search-forward place)
;      (Info-follow-nearest-node))
;    (re-search-forward (concat "-.*: *" name "\\>"))
;    (beginning-of-line)))

(defun maxima-help (&optional arg)
  (interactive "P")
  (let* ((cw (current-word))
         (subj (if arg 
                   cw
                 (completing-read (concat "Maxima help (" cw "): ") 
                                          maxima-symbols nil nil nil nil cw))))
    (condition-case nil
      (maxima-get-info-on-subject subj)
      (error nil))))

(defun maxima-help-at-point ()
  (interactive)
  (maxima-help t))

(defun maxima-apropos (&optional arg)
  "Get help on a certain subject"
  (interactive "P")
  (let* ((cw (current-word))
         (expr (if arg
                   cw
                 (read-string (concat "Maxima help (" cw "): ")
                              nil nil cw)))
         (maxima-help-buffer 
	  (get-buffer-create (concat "*Maxima Help*")))
	 (have-info nil)
	 expr-line
	 (lmark))
    (set-buffer maxima-help-buffer)
    (maxima-remove-kill-buffer-hooks)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Maxima help for " expr "\n\n")
    (insert "[RET] will get help on the subject on the given line\n")
    (insert "q in the *info* buffer will return you here.\n")
    (insert "q in this buffer will exit Maxima help\n\n")
    (with-temp-buffer
      (require 'info)
      (Info-mode)
      (Info-goto-node "(Maxima)Function and Variable Index")
      (goto-char (point-min))
      (search-forward "Menu:")
      (forward-line 1)
      (beginning-of-line)
      (while (re-search-forward (concat "\\*.*" expr ".*:") nil t)
        (setq have-info t)
        (setq expr-line  (buffer-substring-no-properties 
                          (maxima-line-beginning-position) 
                          (maxima-line-end-position)))
        (save-excursion
          (set-buffer maxima-help-buffer)
          (insert expr-line "\n"))))
    (if have-info 
	(progn
	  (set-buffer maxima-help-buffer)
	  (defun maxima-help-subject ()
	    (interactive)
	    (maxima-get-help))
	  (defun maxima-kill-help ()
	    (interactive)
	    (let ((buf (current-buffer)))
	      (delete-window)
              (maxima-remove-kill-buffer-hooks)
	      (kill-buffer buf)))
          (defun maxima-next-subject ()
            (interactive)
            (forward-char 1)
            (if (re-search-forward "^\\*" nil t)
                ()
              (goto-char (point-min))
              (re-search-forward "^\\*" nil t))
            (forward-char -1))
	  (use-local-map (make-sparse-keymap))
	  (define-key (current-local-map) "\C-m" 'maxima-help-subject)
	  (define-key (current-local-map) "q" 'maxima-kill-help)
          (define-key (current-local-map) "\t" 'maxima-next-subject)
	  (goto-char (point-min))
          (re-search-forward "^\\*")
          (forward-char -1)
	  (pop-to-buffer maxima-help-buffer)
          (setq buffer-read-only t))
      (kill-buffer maxima-help-buffer)
      (message (concat "No help for \"" expr "\"")))))

(defun maxima-apropos-at-point ()
  (interactive)
  (maxima-apropos t))

(defun maxima-apropos-help ()
  (interactive)
  (maxima-help-dispatcher nil nil))

(defun maxima-completion-help ()
  (interactive)
  (maxima-help-dispatcher nil t))

(defun maxima-help-dispatcher (&optional arg1 arg2)
  (interactive)
  (cond
   ((or (looking-at "[a-zA-Z_]")
	(looking-at "?[a-zA-Z]")
	(looking-at "%[a-zA-Z]"))
    (if arg2 
	(maxima-context-help)
      (maxima-help (current-word))))
   ((looking-at "?")
    (maxima-get-info-on-subject "\"\\?\""))
   ((looking-at "#")
    (maxima-get-info-on-subject "\"#\""))
   ((looking-at "\\.")
    (maxima-get-info-on-subject "\"\\.\""))
   ((looking-at "[:=!%']")
    (let ((expr (buffer-substring-no-properties
             (maxima-special-symbol-beginning) (maxima-special-symbol-end))))
	  (cond
	   ((or (string= expr "%") (string= expr "%%"))
	    (maxima-get-info-on-subject expr)) ; % and %% are without double quotes
	   ((string= expr "''")
	    (maxima-get-info-on-subject "\"")) ; "''" is called """ in the manual
	   ((or (string= expr ":") (string= expr "::") 
                (string= expr ":=") (string= expr "::=") 
                (string= expr "=") (string= expr "!") (string= expr "!!"))  
	    (maxima-get-info-on-subject (concat "\"" expr "\"")))
	   (t (error "no help for %s" expr)))))
   (arg1
    (error "No help for %s" (char-to-string (char-after (point)))))
   (t					; point is behind a name? 
    (save-excursion
      (progn
	(backward-char 1)
	(maxima-help-dispatcher t arg2))))))

(defun maxima-context-help ()
  (interactive)
  (let* ((stub  (current-word))
	 (completions (all-completions (downcase stub) maxima-symbols)))
    (setq completions 
	  (mapcar 
	   (function upcase) completions))
    (if (member (upcase stub) completions)
	(setq completions (list (upcase stub))))
    (cond ((null completions)
	   (message "No help for %s" stub))
	  ((= 1 (length completions))
	   (maxima-get-info-on-subject (car completions)))
	  (t				; There's no unique completion.
	   (maxima-help-variation completions)))))

(defun maxima-help-variation (completions)
  "Get help on certain subjects."
  (let* ((maxima-help-buffer 
	  (get-buffer-create (concat "*Maxima Help*")))
	 expr-line
	 (lmark))
    (set-buffer maxima-help-buffer)
    (erase-buffer)
    (insert "Maxima help\n")
    (insert "[RET] will get help on the subject on the given line\n")
    (insert "q in the *info* buffer will return you here.\n")
    (insert "q in this buffer will exit Maxima help\n\n")
    (defun maxima-help-insert-line (expr)
      (re-search-forward (concat "\\* " expr ":") nil t)
      (setq expr-line  (buffer-substring-no-properties 
                        (maxima-line-beginning-position) 
                        (maxima-line-end-position)))
      (save-excursion
        (set-buffer maxima-help-buffer)
        (insert expr-line "\n")))
    (with-temp-buffer
      (require 'info nil t)
      (Info-mode)
      (Info-goto-node "(Maxima)Function and Variable Index")
      (goto-char (point-min))
      (search-forward "Menu:")
      (forward-line 1)
      (beginning-of-line)
      (mapcar (function maxima-help-insert-line) completions))
    (goto-char (point-min))
    (defun maxima-help-subject ()
      (interactive)
      (maxima-get-help))
    (defun maxima-kill-help ()
      (interactive)
      (let ((buf (current-buffer)))
	(delete-window)
        (maxima-remove-kill-buffer-hooks)
	(kill-buffer buf)))
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "\C-m" 'maxima-help-subject)
    (define-key (current-local-map) "q" 'maxima-kill-help)
    (goto-char 1)
    (pop-to-buffer maxima-help-buffer)))

(defun maxima-info ()
  "Read the info file for Maxima."
  (interactive)
  (if maxima-running-xemacs
      (info "Maxima")
    (info-other-window "Maxima")))

;;; The help map

(defvar maxima-help-map nil)
(if maxima-help-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'maxima-help)
    (define-key map "d" 'maxima-completion-help)
    (define-key map "\C-d" 'maxima-completion-help)
    (define-key map "i" 'maxima-info)
    (define-key map "\C-i" 'maxima-info)
    (define-key map "m" 'maxima-info)
    (define-key map "\C-m" 'maxima-info)
    (define-key map "a" 'maxima-apropos)
    (define-key map "\C-a" 'maxima-apropos)
    (define-key map "p"  'maxima-apropos-help)
    (define-key map "\C-p"  'maxima-apropos-help)
    (setq maxima-help-map map)))

;;;; Completion

;;; This next functions are from hippie-expand.el
(defun maxima-he-capitalize-first (str)
  (save-match-data
    (if (string-match "\\Sw*\\(\\sw\\).*" str)
	(let ((res (downcase str))
	      (no (match-beginning 1)))
	  (aset res no (upcase (aref str no)))
	  res)
      str)))

(defun maxima-he-ordinary-case-p (str)
  (or (string= str (downcase str))
      (string= str (upcase str))
      (string= str (capitalize str))
      (string= str (maxima-he-capitalize-first str))))


(defun maxima-he-transfer-case (from-str to-str)
  (cond ((string= from-str (substring to-str 0 (min (length from-str)
						    (length to-str))))
	 to-str)
	((not (maxima-he-ordinary-case-p to-str))
	 to-str)
	((string= from-str (downcase from-str))
	 (downcase to-str))
	((string= from-str (upcase from-str))
	 (upcase to-str))
	((string= from-str (maxima-he-capitalize-first from-str))
	 (maxima-he-capitalize-first to-str))
	((string= from-str (capitalize from-str))
	 (capitalize to-str))
	(t
	 to-str)))

;;; The next functions are from comint.el in cvs emacs
(if (and
     (not maxima-running-xemacs)
     (<= emacs-major-version 21)
     (or
      (< emacs-major-version 21)
      (< emacs-minor-version 3)))
(defun comint-dynamic-list-completions (completions)
  "List in help buffer sorted COMPLETIONS.
Typing SPC flushes the help buffer."
  (let ((window (get-buffer-window "*Completions*")))
    (setq completions (sort completions 'string-lessp))
    (if (and (eq last-command this-command)
	     window (window-live-p window) (window-buffer window)
	     (buffer-name (window-buffer window))
	     ;; The above tests are not sufficient to detect the case where we
	     ;; should scroll, because the top-level interactive command may
	     ;; not have displayed a completions window the last time it was
	     ;; invoked, and there may be such a window left over from a
	     ;; previous completion command with a different set of
	     ;; completions.  To detect that case, we also test that the set
	     ;; of displayed completions is in fact the same as the previously
	     ;; displayed set.
	     (equal completions
		    (buffer-local-value 'comint-displayed-dynamic-completions
					(window-buffer window))))
	;; If this command was repeated, and
	;; there's a fresh completion window with a live buffer,
	;; and this command is repeated, scroll that window.
	(with-current-buffer (window-buffer window)
	  (if (pos-visible-in-window-p (point-max) window)
	      (set-window-start window (point-min))
	    (save-selected-window
	      (select-window window)
	      (scroll-up))))
      ;; Display a completion list for the first time.
      (setq comint-dynamic-list-completions-config
	    (current-window-configuration))
      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list completions))
      (message "Type space to flush; repeat completion command to scroll"))
    
    ;; Read the next key, to process SPC.
    (let (key first)
      (if (with-current-buffer (get-buffer "*Completions*")
	    (set (make-local-variable 'comint-displayed-dynamic-completions)
		 completions)
	    (setq key (read-key-sequence nil)
		  first (aref key 0))
	    (and (consp first) (consp (event-start first))
		 (eq (window-buffer (posn-window (event-start first)))
		     (get-buffer "*Completions*"))
		 (eq (key-binding key) 'mouse-choose-completion)))
	  ;; If the user does mouse-choose-completion with the mouse,
	  ;; execute the command, then delete the completion window.
	  (progn
	    (mouse-choose-completion first)
	    (set-window-configuration comint-dynamic-list-completions-config))
	(unless (eq first ?\ )
	  (setq unread-command-events (listify-key-sequence key)))
	(unless (eq first ?\t)
	  (set-window-configuration comint-dynamic-list-completions-config)))))))

;;;
(defun maxima-complete-symbol ()
  "Complete word from list of candidates.
A completions listing will be shown in a help buffer 
if completion is ambiguous."
  (let* ((comint-completion-addsuffix nil)
         (stub  (buffer-substring-no-properties 
                 (maxima-name-beginning) (point)))
	 (completions (all-completions (downcase stub) maxima-symbols)))
    (comint-dynamic-simple-complete stub completions)))

(defun maxima-complete-filename ()
  "Complete the filename."
  (comint-dynamic-complete-filename))

(defun maxima-complete ()
  "Complete the current object, depending on context."
  (interactive)
  (let* ((pmin (maxima-form-beginning-position))
         (pps (parse-partial-sexp pmin (point))))
    (cond 
     ;; complete filename if the point is in a string
     ((nth 3 pps)
      (maxima-complete-filename))
     ;; Otherwise, complete the symbol
     (t
      (maxima-complete-symbol)))))

;; ;;; Use hippie-expand to help with completions
;; (require 'hippie-exp)

;; (defun maxima-he-try (old)
;;   (interactive)
;;   (if (not old)
;;       ;;; let beg be the beginning of the word
;;       (progn
;;         (he-init-string (maxima-name-beginning) (point))
;;         (setq he-expand-list 
;;               (all-completions (downcase he-search-string) maxima-symbols))
;;         (setq he-expand-list 
;;               (mapcar (function 
;;                       (lambda (x) (he-transfer-case he-search-string x)))
;;                       he-expand-list))
;;         (if he-expand-list
;;             (he-substitute-string (car he-expand-list))
;;           nil))
;;     (setq he-expand-list (cdr he-expand-list))
;;     (if he-expand-list
;;         (he-substitute-string (car he-expand-list))
;;       (he-reset-string)
;;       nil)))

;; (fset 'maxima-dynamic-complete
;;       (make-hippie-expand-function '(maxima-he-try)))

;;;; Miscellaneous

(defun maxima-mark-form ()
  "Make the current form as the region."
  (interactive)
  (maxima-goto-beginning-of-form)
  (set-mark (maxima-form-end-position-or-point-max)))

(defun maxima-check-commas (beg end)
  "Check to see if there is a stray comma at the beginning or end."
  (let ((commapt nil))
    (save-excursion
      (goto-char beg)
      (maxima-forward-over-comment-whitespace)
      (if (looking-at ",")
          (setq commapt (point))
        (goto-char end)
        (maxima-back-over-comment-whitespace)
        (when (save-excursion
                (forward-char -1)
                (looking-at "[;$]"))
          (forward-char -1)
          (maxima-back-over-comment-whitespace))
        (forward-char -1)
        (if (looking-at ",")
            (setq commapt (point)))))
    (if commapt
        (progn
          (message "Misplaced comma")
          (goto-char commapt)
          nil)
      t)))

(defun maxima-check-parens (beg end)
  "Check to make sure that the parentheses are balanced in the region."
  (interactive "r")
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (string (buffer-substring-no-properties beg end))
         (keep-going t)
         (match)
         (pt)
         (errmessage nil)
         (parenstack nil))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-mode)
      (maxima-remove-kill-buffer-hooks)
      (modify-syntax-entry ?/ ". 14")
      (modify-syntax-entry ?* ". 23")
      (insert string)
      (goto-char (point-min))
      (while (and (not errmessage)
                  (setq match (maxima-re-search-forward "[][()]" end)))
        (unless (save-excursion 
                  (forward-char -1)
                  (maxima-escaped-char-p))
          (cond
           ((string= match "(")
            (setq parenstack (cons (cons 1 (1- (point))) parenstack)))
           ((string= match "[")
            (setq parenstack (cons (cons 2 (1- (point))) parenstack)))
           ((string= match ")")
            (cond 
             ((not parenstack)
              (setq errmessage "Unmatched close parenthesis")
              (setq pt (1- (point))))
             ((= (caar parenstack) 1)
              (setq parenstack (cdr parenstack)))
             ((= (caar parenstack) 2)
              (setq errmessage "Open bracket closed by parenthesis")
              (setq pt (1- (point))))))
           ((string= match "]")
            (cond 
             ((not parenstack)
              (setq errmessage "Unmatched close bracket")
              (setq pt (1- (point))))
             ((= (caar parenstack) 2)
              (setq parenstack (cdr parenstack)))
             ((= (caar parenstack) 1)
              (setq errmessage "Open parenthesis closed by bracket")
              (setq pt (1- (point))))))))))
    (kill-buffer tmpbuf)
    (cond
     ((not (or parenstack errmessage))
;      (message "Parenthesis and brackets match")
      t)
     (errmessage
      (message errmessage)
      (goto-char (1- (+ beg pt)))
      nil)
     (t
      (cond 
       ((= (caar parenstack) 1)
        (message "Unmatched open parenthesis")
        (goto-char (1- (+ beg (cdar parenstack))))
        nil)
       (t
        (message "Unmatched open bracket")
        (goto-char (+ beg (cdar parenstack)))
        nil))))))


(defun maxima-check-form-parens ()
  "Check to see if the parentheses in the current form are balanced."
  (interactive)
  (maxima-check-parens (maxima-form-beginning-position)
                       (maxima-form-end-position-or-point-max)))

(defun maxima-load-file (file)
  "Prompt for a Maxima file to load."
  (interactive "fMaxima file: ")
  (maxima-string (concat "load(\"" (expand-file-name file) "\");")))

(defun maxima-load-current-file ()
  "Load the current file into Maxima."
  (interactive)
  (maxima-string (concat "load(\"" buffer-file-name "\");")))

;;; For highlighting the region being sent

(defun maxima-mode-add-highlight ()
  (maxima-mode-remove-highlight)
  (if (and maxima-mode-region-begin maxima-mode-region-end)
      (if maxima-running-xemacs
          (progn
            (setq maxima-mode-highlight
                  (make-extent 
                   maxima-mode-region-begin
                   maxima-mode-region-end))
            (set-extent-property maxima-mode-highlight 'face 'highlight))
        (setq maxima-mode-highlight
              (make-overlay
               maxima-mode-region-begin
               maxima-mode-region-end))
        (overlay-put maxima-mode-highlight 'face 'highlight)))
  (setq maxima-mode-region-begin nil)
  (setq maxima-mode-region-end nil))

(defun maxima-mode-remove-highlight ()
  (when maxima-mode-highlight
    (if maxima-running-xemacs
        (delete-extent maxima-mode-highlight)
      (delete-overlay maxima-mode-highlight))
    (setq maxima-mode-highlight nil)))

(defun maxima-mode-add-remove-highlight ()
  (if (or
       (eq this-command 'maxima-send-region)
       (eq this-command 'maxima-send-buffer)
       (eq this-command 'maxima-send-line)
       (eq this-command 'maxima-send-form)
       (eq this-command 'maxima-send-previous-form)
       (eq this-command 'maxima-send-previous-form-and-goto-end-of-form)
       (eq this-command 'maxima-send-full-line)
       (eq this-command 'maxima-send-full-line-and-goto-next-form)
       (eq this-command 'maxima-send-completed-region)
       (eq this-command 'maxima-send-completed-region-and-goto-next-form)
       (eq this-command 'maxima-minibuffer-on-region)
       (eq this-command 'maxima-minibuffer-on-form)
       (eq this-command 'maxima-minibuffer-on-line))
      (maxima-mode-add-highlight)
    (maxima-mode-remove-highlight)))

;;;; Syntax table

(defvar maxima-mode-syntax-table nil "")

(if (not maxima-mode-syntax-table)
    (let ((i 0))
      (setq maxima-mode-syntax-table (make-syntax-table))
      (modify-syntax-entry ?_ "w" maxima-mode-syntax-table)
      (modify-syntax-entry ?% "w" maxima-mode-syntax-table)
      (modify-syntax-entry ?? "w" maxima-mode-syntax-table)
;      (modify-syntax-entry ?\_ "w" maxima-mode-syntax-table)
;;       (while (< i ?0)
;; 	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
;; 	(setq i (1+ i)))
;;       (setq i (1+ ?9))
;;       (while (< i ?A)
;; 	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
;; 	(setq i (1+ i)))
;;       (setq i (1+ ?Z))
;;       (while (< i ?a)
;; 	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
;; 	(setq i (1+ i)))
;;       (setq i (1+ ?z))
;;       (while (< i 128)
;; 	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
;; 	(setq i (1+ i)))
      (modify-syntax-entry ?  "    " maxima-mode-syntax-table)
      (modify-syntax-entry ?\t "   " maxima-mode-syntax-table)
      (modify-syntax-entry ?` "'   " maxima-mode-syntax-table)
      (modify-syntax-entry ?' "'   " maxima-mode-syntax-table)
      (modify-syntax-entry ?, "'   " maxima-mode-syntax-table)
      (modify-syntax-entry ?. "w" maxima-mode-syntax-table)
      (modify-syntax-entry ?# "'   " maxima-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\" maxima-mode-syntax-table)
      (modify-syntax-entry ?/ ". 14" maxima-mode-syntax-table)
      (modify-syntax-entry ?* ". 23" maxima-mode-syntax-table)
      (modify-syntax-entry ?+ "." maxima-mode-syntax-table)
      (modify-syntax-entry ?- "." maxima-mode-syntax-table)
      (modify-syntax-entry ?= "." maxima-mode-syntax-table)
      (modify-syntax-entry ?< "." maxima-mode-syntax-table)
      (modify-syntax-entry ?> "." maxima-mode-syntax-table)
      (modify-syntax-entry ?& "." maxima-mode-syntax-table)
      (modify-syntax-entry ?| "." maxima-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " maxima-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\   " maxima-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " maxima-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " maxima-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " maxima-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " maxima-mode-syntax-table)))


;;;; Keymap

(defvar maxima-mode-map nil
  "The keymap for maxima-mode")

(if maxima-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    ;; Motion
    (define-key map "\M-\C-a" 'maxima-goto-beginning-of-form-interactive)
    (define-key map "\M-\C-e" 'maxima-goto-end-of-form-interactive)
    (define-key map "\M-\C-b" 'maxima-goto-beginning-of-list-interactive)
    (define-key map "\M-\C-f" 'maxima-goto-end-of-list-interactive)
    ;; Process
    (define-key map "\C-c\C-p" 'maxima-display-buffer)
    (define-key map "\C-c\C-r" 'maxima-send-region)
    (define-key map "\C-c\C-b" 'maxima-send-buffer)
    (define-key map "\C-c\C-c" 'maxima-send-line)
    (define-key map "\C-c\C-e" 'maxima-send-previous-form)
    (define-key map "\C-c\C-s" 'maxima-send-previous-form-and-goto-end-of-form)
    (define-key map [(control return)] 
      'maxima-send-full-line-and-goto-next-form)
    (define-key map [(meta return)] 
      'maxima-send-completed-region-and-goto-next-form)
    (define-key map [(control meta return)] 'maxima-send-buffer)
    (define-key map "\C-c\C-k" 'maxima-stop)
    (define-key map "\C-c\C-q" 'maxima-clear-queue)
    (define-key map "\C-c\C-l" 'maxima-load-file)
    (define-key map "\C-c\C-f" 'maxima-load-current-file)
    ;; Completion
    ;(if maxima-use-dynamic-complete
    ;    (define-key map (kbd "M-TAB") 'maxima-dynamic-complete)        
    (define-key map (kbd "M-TAB") 'maxima-complete)
    ;; Commenting
    (define-key map "\C-c;" 'comment-region)
    (define-key map "\C-c:" 'maxima-uncomment-region)
    (define-key map "\M-;" 'maxima-insert-short-comment)
    (define-key map "\C-c*" 'maxima-insert-long-comment)
    ;; Indentation
;    (define-key map "\t" 'maxima-reindent-line)
    (define-key map "\C-m" 'maxima-return)
    (define-key map "\M-\C-q" 'maxima-indent-form)
;    (define-key map [(control tab)] 'maxima-untab)
    ;; Help
    (define-key map "\C-c\C-d" maxima-help-map)
    (define-key map [(f12)] 'maxima-help)
    (define-key map [(meta f12)] 'maxima-apropos)
    ;; Minibuffer
    (define-key map "\C-c\C-nr" 'maxima-minibuffer-on-region)
    (define-key map "\C-c\C-nl" 'maxima-minibuffer-on-line)
    (define-key map "\C-c\C-nf" 'maxima-minibuffer-on-form)
    ;; Misc
    (define-key map "\M-h" 'maxima-mark-form)
    (define-key map "\C-c\)" 'maxima-check-parens)
    (define-key map [(control c) (control \))] 'maxima-check-form-parens)
;    (define-key map "\C-cC-\)" 'maxima-check-form-parens)
    (define-key map "\177" 'backward-delete-char-untabify)
    (setq maxima-mode-map map)))

;;;; Menu

(easy-menu-define maxima-mode-menu maxima-mode-map "Maxima mode menu"
  '("Maxima"
    ("Motion"
     ["Beginning of form" maxima-goto-beginning-of-form-interactive t]
     ["End of form" maxima-goto-end-of-form-interactive t]
     ["Beginning of sexp" maxima-goto-beginning-of-list-interactive t]
     ["End of sexp" maxima-goto-end-of-list-interactive t])
    ("Process"
     ["Start process" maxima-start t]
     ["Send region" maxima-send-region t]
     ["Send buffer" maxima-send-buffer t]
     ["Send line" maxima-send-line t]
     ["Send form" maxima-send-form t]
     ["Load file" maxima-load-file t]
     "----"
     ["Display buffer" maxima-display-buffer t]
     "----"
     ["Kill process" maxima-stop t])
    ("Indentation"
;     ["Change to basic" (maxima-change-indent-style "b")  
;      (not (eq maxima-newline-style 'basic))]
     ["Change to standard" (maxima-change-indent-style "s")  
      (not (eq maxima-indent-style 'standard))]
     ["Change to smart" (maxima-change-indent-style "p")  
      (not (eq maxima-indent-style 'perhaps-smart))])
    ("Misc"
     ["Mark form" maxima-mark-form t]
     ["Check parens in region" maxima-check-parens t]
     ["Check parens in form" maxima-check-form-parens t]
     ["Comment region" comment-region t]
     ["Uncomment region" maxima-uncomment-region t])
    ("Help"
     ["Maxima info" maxima-info t]
     ["Help" maxima-help t])))


;;;; Variable setup
;;;; (These are used in both maxima-mode and inferior-maxima-mode).

(defvar maxima-mode-abbrev-table nil "")

(defun maxima-mode-variables ()
  "Sets all the necessary variables for maxima-mode"
  (set-syntax-table maxima-mode-syntax-table)
  (setq local-abbrev-table maxima-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'maxima-indent-line)
  (make-local-variable 'indent-tabs-mode)
  (unless maxima-use-tabs
    (setq indent-tabs-mode nil))
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (make-local-variable 'comment-start)
  (setq comment-start "/*")
  (make-local-variable 'comment-end)
  (setq comment-end "*/")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'maxima-comment-indent)
  (setq imenu-generic-expression 
        (list '(nil "^ *\\([a-zA-Z0-9_]*\\) *(.*) *:=" 1))))


;;;; Maxima mode

(defun maxima-mode ()
  "Major mode for editing Maxima code.

Maxima mode provides the following motion commands:
\\[maxima-goto-beginning-of-form-interactive]: Move to the beginning of the form.
\\[maxima-goto-end-of-form-interactive]: Move to the end of the form.
\\[maxima-goto-beginning-of-list-interactive]: Move to the beginning of the sexp.
\\[maxima-goto-end-of-list-interactive]: Move to the end of the sexp.

and the following miscellaneous commands.
\\[maxima-mark-form]: Mark the current form
\\[maxima-check-parens]: Check the current region for balanced parentheses.
\\[maxima-check-form-parens]: Check the current form for balanced parentheses.

Maxima mode has the following completions commands:
M-TAB: Complete the Maxima symbol as much as possible, providing
     a completion buffer if there is more than one possible completion.

Portions of the buffer can be sent to a Maxima process.  (If a process is 
not running, one will be started.)
\\[maxima-send-region]: Send the region to Maxima.
\\[maxima-send-buffer]: Send the buffer to Maxima.
\\[maxima-send-line]: Send the line to Maxima.
\\[maxima-send-form]: Send the form to Maxima.
\\[maxima-send-full-line-and-goto-next-form]: Send the smallest set of lines which contains
   the cursor and contains no incomplete forms, and go to the next form.
\\[maxima-send-completed-region-and-goto-next-form]:  As above, but with
   the region instead of the current line.
\\[maxima-load-file] will prompt for a filename and load it into Maxima
When something is sent to Maxima, a buffer running an inferior Maxima 
process will appear.  It can also be made to appear by using the command
\\[maxima-display-buffer].
If an argument is given to a command to send information to Maxima,
the region (buffer, line, form) will first be checked to make sure
the parentheses are balanced.
The Maxima process can be killed, after asking for confirmation 
with \\[maxima-stop].  To kill without confirmation, give \\[maxima-stop]
an argument.

By default, indentation will be to the same level as the 
previous line, with an additional space added for open parentheses.
The behaviour of indent can be changed by the command 
\\[maxima-change-indent-style].  The possibilities are:
Standard:      Standard indentation.
Perhaps smart: Tries to guess an appropriate indentation, based on
               open parentheses, \"do\" loops, etc.
The default can be set by setting the value of the variable 
\"maxima-indent-style\" to either 'standard or 'perhaps-smart.
In both cases, \\[maxima-untab] will remove a level of indentation.

To get help on a Maxima topic, use:
\\[maxima-help].
To read the Maxima info manual, use:
\\[maxima-info].
To get help with the symbol under point, use:
\\[maxima-completion-help].
To get apropos with the symbol under point, use:
\\[maxima-apropos-help].

\\{maxima-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'maxima-mode)
  (setq mode-name "Maxima")
  (use-local-map maxima-mode-map)
  (maxima-mode-variables)
  (cond
   ((eq maxima-newline-style 'basic)
    (setq maxima-indent-style 'standard))
   ((eq maxima-newline-style 'standard)
    (setq maxima-indent-style 'standard))
   ((eq maxima-newline-style 'perhaps-smart)
    (setq maxima-indent-style 'perhaps-smart)))
  (easy-menu-add maxima-mode-menu maxima-mode-map)
  (if maxima-running-xemacs
      (add-local-hook 'post-command-hook
                      'maxima-mode-add-remove-highlight)
    (add-hook 'post-command-hook
              'maxima-mode-add-remove-highlight nil t))
  (run-hooks 'maxima-mode-hook))

(define-derived-mode maxima-noweb-mode maxima-mode
  "Maxima Noweb Mode"
  (setq maxima-mode-type 'maxima-noweb-mode))

;;;; Interacting with the Maxima process

;;; Checking on the process
(defun inferior-maxima-running ()
  (and (processp inferior-maxima-process)
       (eq (process-status inferior-maxima-process) 'run)))

;;; Sending the information
(defun inferior-maxima-get-old-input ()
  (let (pt pt1)
    (save-excursion
      (if (re-search-forward 
           (concat "\\(^(\\(" maxima-outchar "\\|" maxima-linechar "\\)[0-9]*) \\)")
           nil 1)
          (goto-char (match-beginning 0)))
      (skip-chars-backward " \t\n")
      (setq pt (point)))
    (save-excursion
      (re-search-backward inferior-maxima-prompt)
      (setq pt1 (match-end 0)))
    (buffer-substring-no-properties pt1 pt)))

(defun inferior-maxima-comint-send-input (&optional query)
  "Take note of position, then send the input"
  (unless query
    (setq inferior-maxima-input-end (point)))
  (setq inferior-maxima-waiting-for-output t)
  (comint-send-input))

;;; This next function is a modified version of comint-strip-ctrl-m
(defun inferior-maxima-remove-double-prompt (&optional string)
  "Fix the double prompt that occasionally appears in XEmacs."
  (let ((pmark (process-mark inferior-maxima-process))
	(pos))
    (set-buffer (process-buffer inferior-maxima-process))
    (setq pos comint-last-output-start)
    (if (marker-position pos)
	(save-excursion
	  (goto-char pos)
          (beginning-of-line)
	  (while (re-search-forward 
                  (concat "(" maxima-inchar "[0-9]+).*\r") pmark t)
	    (replace-match "" t t))))))

(defun inferior-maxima-remove-double-input-prompt (&optional string)
  "Fix the double prompt that occasionally appears in Emacs."
  (let ((pmark (process-mark inferior-maxima-process))
	(pos))
    (save-excursion
      (set-buffer (process-buffer inferior-maxima-process))
      (goto-char inferior-maxima-input-end)
      (forward-line 1)
      (if (looking-at (concat "(" maxima-inchar "[0-9]+)"))
          (kill-line 1))
      (if (looking-at "")
          (delete-char 1)))))

;;; This next function will replace tabs in the output by spaces
;; untabify isn't defined in xemacs
(unless (fboundp 'untabify)
(defun untabify (start end)
  "Convert all tabs in region to multiple spaces, preserving columns.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) end)
      (goto-char start)
      (while (search-forward "\t" nil t)	; faster than re-search
	(forward-char -1)
	(let ((tab-beg (point))
	      (indent-tabs-mode nil)
	      column)
	  (skip-chars-forward "\t")
	  (setq column (current-column))
	  (delete-region tab-beg (point))
	  (indent-to column)))))))

(defun inferior-maxima-replace-tabs-by-spaces (&optional string)
  "Replace tabs in the Maxima output by spaces."
  (let ((beg))
    (set-buffer (process-buffer inferior-maxima-process))
    (if (marker-position comint-last-output-start)
        (setq beg comint-last-output-start)
      (setq beg (point-min)))
    (untabify beg
              (process-mark inferior-maxima-process))))

(defun inferior-maxima-wait-for-output ()
  "Wait for output from the Maxima process."
  (when (and 
          inferior-maxima-waiting-for-output
          (inferior-maxima-running))
    (accept-process-output inferior-maxima-process))
  (if maxima-running-xemacs
      (sleep-for 0.1)
    (sit-for 0 inferior-maxima-after-output-wait)))

(defun inferior-maxima-output-filter (str)
  "Look for a new input prompt"
  (cond ((and
          (string-match "? *$" str)
          (not (string-match (concat "(" maxima-outchar "[0-9]+)") str)))
         (maxima-ask-question str))
        ((string-match inferior-maxima-prompt str)
         (if (and inferior-maxima-process (not (string= maxima-block "")))
             (maxima-single-string (maxima-get-command))
           (if (not inferior-maxima-process)
               (maxima-clear-queue))
           (setq inferior-maxima-waiting-for-output nil)))))

(defun inferior-maxima-sentinel (proc state)
  "Write the input history when the process ends"
  (unless (string-match "^run" state)
    (comint-write-input-ring)))

(defun maxima-start ()
  "Start the Maxima process."
  (interactive)
  (if (processp inferior-maxima-process)
      (unless (eq (process-status inferior-maxima-process) 'run)
        (delete-process inferior-maxima-process)
        (if (get-buffer "*maxima*")
            (save-excursion
              (set-buffer "*maxima*")
              (erase-buffer)))
        (setq inferior-maxima-process nil)))
  (unless (processp inferior-maxima-process)
    (setq inferior-maxima-input-end 0)
    (setq inferior-maxima-waiting-for-output t)
    (let ((mbuf)
          (cmd))
      (setq mbuf (apply #'make-comint "maxima" maxima-command nil maxima-args))
      (save-excursion
        (set-buffer mbuf)
        (setq inferior-maxima-process (get-buffer-process mbuf))
        (add-hook 'comint-output-filter-functions
                  'inferior-maxima-output-filter nil t)
        (add-hook 'comint-output-filter-functions
                  'inferior-maxima-replace-tabs-by-spaces nil t)
;        (add-hook 'comint-output-filter-functions
;                  'inferior-maxima-remove-double-input-prompt nil t)
	(if maxima-fix-double-prompt
            (add-hook 'comint-output-filter-functions
                      'inferior-maxima-remove-double-prompt nil t))
        (inferior-maxima-wait-for-output)
        (inferior-maxima-mode)))))

(defun maxima-stop (&optional arg)
  "Kill the currently running Maxima process."
  (interactive "P")
  (if (processp inferior-maxima-process)
      (if arg
	  (progn 
	    (delete-process inferior-maxima-process)
	    (kill-buffer "*maxima*")
	    (setq inferior-maxima-process nil))
	(if (y-or-n-p "Really quit Maxima? ")
	    (progn
	      (delete-process inferior-maxima-process)
	      (kill-buffer "*maxima*")
	      (setq inferior-maxima-process nil))))))

;;; Sending information to the process

(defun maxima-single-string (string)
  "Send a string to the Maxima process."
  (setq string (maxima-strip-string-add-semicolon string))
  (maxima-start)
;  (inferior-maxima-wait-for-output)
  (save-current-buffer
    (set-buffer (process-buffer inferior-maxima-process))
    (goto-char (point-max))
    (insert string)
    (inferior-maxima-comint-send-input)
    (goto-char (point-max))))

(defun maxima-ask-question (string)
  "Ask the question maxima wants answered."
  (let ((ans (read-string 
              (concat (maxima-strip-string string) " " ))))
    (unless (string-match "[;$]" ans)
      (setq ans (concat ans ";")))
    (setq ans (maxima-strip-string ans))
    (save-current-buffer
      (set-buffer (process-buffer inferior-maxima-process))
      (goto-char (point-max))
      (insert ans)
      (inferior-maxima-comint-send-input t)
      (goto-char (point-max)))))

(defun maxima-get-command (&optional arg)
  "Return the maxima command that's at the front of maxima-block.
Remove it from the front of maxima-block.
With an argument, use maxima-block-wait instead of maxima-block."
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (pt)
         (command))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-remove-kill-buffer-hooks)
      (if arg
          (insert maxima-block-wait)
        (insert maxima-block))
      (goto-char (point-min))
      (maxima-forward-over-comment-whitespace)
      (setq pt (point))
      (if (string-match "[$;]\\|:lisp"
                          (buffer-substring-no-properties (point) (point-max)))
          (progn
            (if (looking-at ":lisp")
                (progn
                  (search-forward ":lisp")
                  (forward-sexp)
                  (setq command (buffer-substring-no-properties pt (point))))
              (maxima-goto-end-of-form)
              (setq command (buffer-substring-no-properties pt (point))))
            (maxima-forward-over-comment-whitespace)
            (if arg
                (setq maxima-block-wait
                      (maxima-strip-string-add-semicolon
                       (buffer-substring-no-properties (point) (point-max))))
              (setq maxima-block 
                    (maxima-strip-string-add-semicolon
                     (buffer-substring-no-properties (point) (point-max)))))
            (setq command (buffer-substring-no-properties pt (point))))
        (if arg
            (setq maxima-block-wait "")
          (setq maxima-block "")))
      (if arg
          (if (string= maxima-block-wait ";") (setq maxima-block-wait ""))
        (if (string= maxima-block ";") (setq maxima-block "")))
      (kill-buffer tmpbuf))
    command))

(defun maxima-send-block (stuff)
  "Send a block of code to Maxima."
  (maxima-start)
  (setq stuff (maxima-strip-string-add-semicolon stuff))
;  (unless (string-match (substring stuff -1) ";$")
;    (setq stuff (concat stuff ";")))
  (if (string= maxima-block "")
      (progn
        (setq maxima-block stuff)
        (maxima-single-string (maxima-get-command)))
    (setq maxima-block (concat maxima-block stuff))))

(defun maxima-send-block-wait (stuff)
  "Send a block of code to Maxima; wait for it to finish.
Return the last string sent."
  (maxima-start)
  (if (not (string= maxima-block ""))
      (message "Maxima process currently busy.")
    (setq maxima-block-wait (maxima-strip-string-add-semicolon stuff))
    (while (not (string= maxima-block-wait ""))
      (maxima-single-string-wait (maxima-get-command t)))))

(defun maxima-clear-queue ()
  "Clear out the queue of commands to send to the maxima process."
  (interactive)
  (setq maxima-block "")
  (setq maxima-block-wait ""))

;;; Getting information back from Maxima.

(defun maxima-last-output ()
  "Get the most recent output from Maxima."
  (interactive)
  (inferior-maxima-wait-for-output)
  (save-excursion
    (set-buffer (process-buffer inferior-maxima-process))
    (let* ((pt (point))
           (pmark (progn (goto-char (process-mark inferior-maxima-process))
                         (forward-line 0)
                         (point-marker)))
           (beg (progn
                  (goto-char inferior-maxima-input-end)
                  (forward-line 1)
                  (point)))
           (output (buffer-substring-no-properties beg pmark)))
      (goto-char pt)
      output)))

(defun maxima-last-output-noprompt ()
  "Return the last Maxima output, without the prompts"
  (interactive)
  (if (not (inferior-maxima-running))
      (maxima-last-output)
    (let* ((output (maxima-last-output))
           (newstring)
           (i 0)
           (beg)
           (end)
           (k))
    ;; Replace the output prompt with spaces
      (setq beg (string-match 
                 (concat "\\(^(" maxima-outchar "[0-9]*) \\)") output))
      (if (not beg)
          output
        (setq end (1+ (string-match ")" output beg)))
        (setq newstring (substring output 0 beg))
        (setq k (- end beg))
        (while (< i k)
          (setq newstring (concat newstring " "))
          (setq i (1+ i)))
        (concat newstring 
                (substring output 
                           end))))))

(defun maxima-last-output-tex-noprompt ()
  "Return the last Maxima output, between the dollar signs."
  (interactive)
  (let* ((output (maxima-last-output))
         (begtex (string-match "\\$\\$" output))
         (endtex (string-match "\\$\\$" output (1+ begtex))))
    (concat
     (substring output begtex (+ endtex 2))
     "\n")))


;;; Sending information to the process should be done through these
;; next five commands

(defun maxima-single-string-wait (string)
  "Send a single string to the maxima process,
waiting for output after."
  (inferior-maxima-wait-for-output)
  (maxima-single-string string)
  (inferior-maxima-wait-for-output))

(defun maxima-string (string)
  "Send a string to the Maxima process."
  (maxima-send-block string))

(defun maxima-region (beg end)
  "Send the region to the Maxima process."
  (setq maxima-mode-region-begin beg)
  (setq maxima-mode-region-end end)
  (maxima-string
   (buffer-substring-no-properties beg end)))

;;; Some functions to send commands to the process.

(defun maxima-send-region (beg end &optional arg)
  "Send the current region to the Maxima process.
With an argument, don't check the parentheses first."
  (interactive "r\nP")
  (if arg
    (maxima-region beg end)
    (if (maxima-check-parens beg end)
        (maxima-region beg end)))
  (maxima-display-buffer))

(defun maxima-send-buffer (&optional arg)
  "Send the buffer to the Maxima process, after checking the parentheses.
With an argument, don't check the parentheses."
  (interactive "P")
  (maxima-send-region (point-min) (point-max) arg))

(defun maxima-send-line (&optional arg)
  "Send the current line to the Maxima process, after checking parentheses.
With an argument, don't check parentheses."
  (interactive "P")
  (let ((b (maxima-line-beginning-position))
	(e (maxima-line-end-position)))
    (maxima-send-region b e arg)))

(defun maxima-send-form (&optional arg)
  "Send the current form to the Maxima process, checking parentheses.
With an argument, don't check parentheses."
  (interactive "P")
  (maxima-send-region (maxima-form-beginning-position)
                      (maxima-form-end-position-or-point-max) arg))

(defun maxima-send-previous-form (&optional arg)
  "Send the previous form to the Maxima process, checking parentheses.
With an argument, don't check parentheses."
  (interactive "P")
  (save-excursion
    (if (maxima-re-search-backward "[;$]")
        (maxima-send-region (maxima-form-beginning-position)
                            (maxima-form-end-position-or-point-max) arg)
      (message "No previous form."))))

(defun maxima-send-previous-form-and-goto-end-of-form (&optional arg)
  "Send the previous form to the Maxima process and go to the end of form."
  (interactive "P")
  (maxima-send-previous-form arg)
  (maxima-goto-end-of-form-interactive))

(defun maxima-send-full-line ()
  "Send the minimum number of lines such that the current is one of them,
and such that no line contains an incomplete form."
  (interactive)
  (let ((beg (point)) (end (point)))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (maxima-goto-beginning-of-form)
      (while (< (point) beg) 
	(progn 
	  (beginning-of-line)
	  (setq beg (point))
	  (maxima-goto-beginning-of-form)))
      (goto-char end)
      (end-of-line)
      (setq end (point))
      (while (and (< (maxima-form-beginning-position) end) (< end (point-max)))
	(progn
	  (forward-line 1)
	  (end-of-line)
	  (setq end (point))))
      (skip-chars-backward " \t;$")
      (if (re-search-forward "[;$]" end t)
	  (maxima-send-region beg (point))
	(error "No ; or $ at end"))
      end)))

(defun maxima-send-full-line-and-goto-next-form ()
  "Do a maxima-send-full-line and go to the beginning of the next form."
  (interactive)
  (goto-char (maxima-send-full-line))
  (maxima-goto-beginning-of-form))

(defun maxima-send-completed-region (beg end)
  "Send the marked region, but complete possibly non-complete forms at the bounderies."
  (interactive "r\nP")
  (let ((beg1)
        (end1))
    (save-excursion
      (goto-char beg)
      (setq beg1 (maxima-form-beginning-position))
      (goto-char end)
      (setq end1 (maxima-form-end-position-or-point-max))
      (maxima-send-region beg1 end1)
      end1)))

(defun maxima-send-completed-region-and-goto-next-form (beg end)
  "Do a maxima-send-completed-region and go to the beginning of the next form."
  (interactive "r\nP")
  (goto-char (maxima-send-completed-region beg end))
  (maxima-goto-beginning-of-form))

(defun maxima-display-buffer ()
  "Display the inferior-maxima-process buffer so the recent output is visible."
  (interactive)
  (let ((origbuffer (current-buffer)))
    (if (not (processp inferior-maxima-process))
	(maxima-start))
    (pop-to-buffer (process-buffer inferior-maxima-process))
    (goto-char (point-max))
;    (recenter (universal-argument))
    (pop-to-buffer origbuffer)))


;;;; The inferior Maxima process

;;; Completions from previous input

;; First, a function to take the comint-input-ring and return a 
;; list of previous inputs

(defun inferior-maxima-previous-inputs ()
  "Return a list of previous inputs."
  (interactive)
  (let* ((inputs nil)
         (comint-inputs (cddr comint-input-ring))
         (i 0))
    (while (and (< i comint-input-ring-size) 
                (not (null (aref comint-inputs i))))
      (unless (member (aref comint-inputs i) inputs)
        (setq inputs (cons (aref comint-inputs i) inputs)))
      (setq i (1+ i)))
    (reverse inputs)))

(defun inferior-maxima-input-complete ()
  "Complete line from list of previous input."
  (interactive)
  (let* ((stub  (buffer-substring-no-properties 
                 (inferior-maxima-bol-position) (point)))
	 (completions (all-completions (downcase stub) 
                                       (inferior-maxima-previous-inputs))))
    (setq completions 
          (mapcar 
           (function (lambda (x) (maxima-he-transfer-case stub x))) completions))
    (cond ((null completions)
	   (message "No completions of %s" stub))
	  ((= 1 (length completions))	; Gotcha!
	   (let ((completion (car completions)))
	     (if (string-equal completion stub)
		 (message "Sole completion")
	       (insert (substring completion (length stub)))
	       (message "Completed"))))
	  (t				; There's no unique completion.
             (comint-dynamic-list-completions completions)))))

(defun inferior-maxima-complete ()
  "Complete the current object, depending on context."
  (interactive)
  (let* ((pmin (save-excursion
                 (re-search-backward inferior-maxima-prompt)
                 (point)))
         (pps (parse-partial-sexp pmin (point))))
    (cond 
     ;; complete filename if the point is in a string
     ((nth 3 pps)
      (maxima-complete-filename))
     ;; Otherwise, complete the symbol
     (t
      (maxima-complete-symbol)))))

;; (defun maxima-input-he-try (old)
;;   (interactive)
;;   (if (not old)
;;       ;;; let beg be the beginning of the word
;;       (progn
;;         (he-init-string (inferior-maxima-bol-position) (point))
;;         (setq he-expand-list 
;;               (all-completions (downcase he-search-string) (maxima-previous-inputs)))
;;         (setq he-expand-list 
;;               (mapcar (function 
;;                       (lambda (x) (he-transfer-case he-search-string x)))
;;                       he-expand-list))
;;         (if he-expand-list
;;             (he-substitute-string (car he-expand-list))
;;           nil))
;;     (setq he-expand-list (cdr he-expand-list))
;;     (if he-expand-list
;;         (he-substitute-string (car he-expand-list))
;;       (he-reset-string)
;;       nil)))

;; (fset 'maxima-dynamic-input-complete
;;       (make-hippie-expand-function '(maxima-input-he-try)))

;;; Sending a line to the process while in the process buffer

(defun inferior-maxima-check-and-send-line ()
  "Check the lines for mis-matched parentheses, then send the line."
  (interactive)
  (let ((ok nil)
	(pt (point))
	pt1)
    (save-excursion
      (end-of-line)
      (skip-chars-backward " \t")
      (forward-char -1)
      (when (looking-at "[$;]")
        (setq pt (point))
        (setq ok t)))
    (if ok
	(progn
	  (save-excursion
	    (re-search-backward inferior-maxima-prompt)
	    (setq pt1 (match-end 0)))
	  (if (maxima-check-parens pt1 pt)
              (inferior-maxima-comint-send-input)))
      (inferior-maxima-comint-send-input))))

(defun inferior-maxima-send-line ()
  "Send the line to the Maxima process."
  (interactive)
  (inferior-maxima-comint-send-input))

(defun inferior-maxima-bol ()
  "Go to the beginning of the line, but past the prompt."
  (interactive)
  (let ((eol (save-excursion (end-of-line) (point))))
    (forward-line 0)
    (if (and (looking-at inferior-maxima-prompt)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))

(defun inferior-maxima-bol-position ()
  (save-excursion
   (inferior-maxima-bol)
   (point)))


;;;; Inferior Maxima mode

(define-derived-mode inferior-maxima-mode 
                     comint-mode
                     "Inferior Maxima"
  "Major mode for interacting with an inferior Maxima process.

Return will check the line for balanced parentheses, and send line as input.
Control return will send the line as input without checking for balanced
parentheses.

M-TAB will complete the Maxima symbol as much as possible, providing
     a completion buffer if there is more than one possible completion.

\\[maxima-smart-complete] will complete the input line, based on previous input lines.
\\[maxima-help] will get help on a Maxima topic.
\\[maxima-info] will bring up the Maxima info manual.
\\[maxima-stop] will kill the process and the buffer, after asking for
  confirmation.  To kill without confirmation, give \\[maxima-stop] an
  argument.

To scroll through previous commands,
\\[comint-previous-input] will bring the previous input to the current prompt,
\\[comint-next-input] will bring the next input to the prompt.
\\[comint-previous-matching-input] will bring the previous input matching
  a regular expression to the prompt,
\\[comint-next-matching-input] will bring the next input matching
  a regular expression to the prompt.
"
;  (if maxima-use-full-color-in-process-buffer
;      (inferior-maxima-font-setup))
  (setq comint-prompt-regexp inferior-maxima-prompt)
  (setq comint-get-old-input (function inferior-maxima-get-old-input))
  (setq mode-line-process '(": %s"))
  (maxima-mode-variables)
  (setq tab-width 8)
  (if (and (not maxima-running-xemacs) (< emacs-major-version 21))
      (make-local-hook 'kill-buffer-hook))
  (if maxima-running-xemacs
      (add-local-hook 'kill-buffer-hook
                      (function
                       (lambda ()
                         (maxima-clear-queue)
                         (if (processp inferior-maxima-process)
                             (delete-process inferior-maxima-process))
                         (setq inferior-maxima-process nil)
                         (run-hooks 'inferior-maxima-exit-hook))))
    (add-hook 'kill-buffer-hook
              (function
               (lambda ()
                 (maxima-clear-queue)
                 (if (processp inferior-maxima-process)
                     (delete-process inferior-maxima-process))
                 (setq inferior-maxima-process nil)
                 (run-hooks 'inferior-maxima-exit-hook))) t t))
  (setq comint-input-ring-size maxima-input-history-length)
  (if maxima-save-input-history
      (progn
        (setq comint-input-ring-file-name maxima-input-history-file)
        (comint-read-input-ring t)
        (set-process-sentinel inferior-maxima-process
                              'inferior-maxima-sentinel)))
  (set (make-local-variable 'comint-prompt-read-only) t)
  (run-hooks 'inferior-maxima-mode-hook))

;;;; Keymap

(define-key inferior-maxima-mode-map "\C-a" 
  'inferior-maxima-bol)
(define-key inferior-maxima-mode-map "\C-m"  
  'inferior-maxima-check-and-send-line)
(define-key inferior-maxima-mode-map [(control return)] 
  'inferior-maxima-send-line)
(define-key inferior-maxima-mode-map [(meta control tab)] 
  'inferior-maxima-input-complete)
(define-key inferior-maxima-mode-map "\e\t" 'inferior-maxima-complete)
(define-key inferior-maxima-mode-map "\177" 'backward-delete-char-untabify)
(define-key inferior-maxima-mode-map "\C-c\C-k" 'maxima-stop)
(define-key inferior-maxima-mode-map "\C-c\C-d" maxima-help-map)

;;;; Menu

(easy-menu-define inferior-maxima-mode-menu inferior-maxima-mode-map 
  "Maxima mode menu"
  '("Maxima"
    ("Help"
     ["Maxima info" maxima-info t]
     ["Help" maxima-help t])
    ("Quit"
     ["Kill process" maxima-stop t])))

;;;; Running Maxima

(defun maxima ()
  "Run Maxima interactively inside a buffer."
  (interactive)
  (maxima-start)
  (switch-to-buffer (process-buffer inferior-maxima-process)))

;;; Interacting with Maxima outside of a maxima buffer

(defun maxima-minibuffer ()
  "Communicate with Maxima through the minibuffer"
  (interactive)
  (maxima-start)
  (let ((input (read-string "Maxima: " nil maxima-minibuffer-history))
        (output nil)
        (twod (and maxima-minibuffer-2d (not maxima-running-xemacs))))
    (setq input (maxima-strip-string-add-semicolon input))
    (if twod
        (maxima-single-string-wait 
         "block(emacsdisplay:display2d,display2d:true,linenum:linenum-1,%);")
      (maxima-single-string-wait
       "block(emacsdisplay:display2d,display2d:false,linenum:linenum-1,%);"))
    (maxima-single-string-wait input)
    (setq output (maxima-last-output-noprompt))
    (maxima-single-string-wait "block(display2d:emacsdisplay,linenum:linenum-1,%);")
    (if (not twod)
        (setq output (maxima-remove-whitespace-from-ends output))
      ;; Strip the beginning and trailing newline
      (while (string-match "\\` *\n" output)
        (setq output (substring output (match-end 0))))
      (while (string-match "\n *\\'" output)
        (setq output (substring output 0 (match-beginning 0)))))
    (setq output (maxima-replace-in-string "%" "%%" output))
    (message output)))

(defun maxima-minibuffer-delete-output (beg end)
  (let ((mmom (maxima-minor-output-mark))
        (mmoe (maxima-minor-output-mark-end)))
    (if (or 
         (and (string-match maxima-minor-mode-bad-delimiter-regexp mmom)
              (string= (match-string 0 mmom) mmom))
         (and (string-match maxima-minor-mode-bad-delimiter-regexp mmoe)
              (string= (match-string 0 mmoe) mmoe)))
        (message "Old output not deleted (improper delimiter).")
      (let (pt)
        (save-excursion
          (goto-char beg)
          (if (search-forward mmom end t)
              (progn
                (setq pt (match-beginning 0))
                (search-forward mmoe)
                (kill-region pt (point)))
            (goto-char end)
            (if (looking-at (concat "[ \n]*" (regexp-quote mmom)))
                (progn
                  (search-forward mmoe)
                  (kill-region end (point)))))
          (point))))))

(defun maxima-minibuffer-on-region (beg end &optional arg)
  "Send the current region to Maxima; display last output in minibuffer.
With an argument, insert \" ==> \" into the current buffer,
followed by the output, followed by \"\\\".  In this case, any previous output 
will be deleted."
  (interactive "r\nP")
  (let ((output nil)
        (minibufferoutput)
        (input)
        (realend nil)
        (realbeg)
        (outputbeg)
        (delreg)
        (delregbeg)
        (delregend)
        (twod (and maxima-minibuffer-2d (not maxima-running-xemacs))))
    (save-excursion
      (goto-char beg)
      (maxima-forward-over-comment-whitespace)
      (setq realbeg (point))
      (if (re-search-forward (maxima-minor-output-mark) end t)
          (setq realend  
                (if (eq major-mode 'maxima-mode)
                    (- (point) (length maxima-mode-minor-output))
                  (- (point) (length maxima-minor-output))))
        (goto-char end)
        (maxima-back-over-comment-whitespace)
        (setq realend (point))))
    (setq input (maxima-strip-string-add-semicolon
                 (buffer-substring-no-properties realbeg realend)))
    (if arg
        (maxima-minibuffer-delete-output beg end))
    (setq maxima-minor-mode-region-begin realbeg)
    (setq maxima-minor-mode-region-end realend)
    (when (or (not maxima-minor-mode-check-input)
              (and
               (maxima-check-parens realbeg realend)
               (maxima-check-commas realbeg realend)))
      (maxima-start)
      (if twod
          (maxima-single-string-wait 
           "block(emacsdisplay:display2d,display2d:true,linenum:linenum-1,%);")
        (maxima-single-string-wait
         "block(emacsdisplay:display2d,display2d:false,linenum:linenum-1,%);"))
      (maxima-send-block-wait input)
      (setq output (maxima-last-output-noprompt))
      (maxima-single-string-wait "block(display2d:emacsdisplay,linenum:linenum-1,%);")
      (if (not twod)
          (setq output (maxima-remove-whitespace-from-ends output))
        ;; Strip the beginning and trailing newline
        (while (string-match "\\` *\n" output)
          (setq output (substring output (match-end 0))))
        (while (string-match "\n *\\'" output)
          (setq output (substring output 0 (match-beginning 0)))))
      (unless arg
        (setq minibufferoutput (maxima-replace-in-string "%" "%%" output))
        (message minibufferoutput))
      (if (and arg
               (not twod))
          (save-excursion
            (goto-char realend)
            (if (looking-at "^")
                (setq realend (1- realend)))
            ;(delete-region realend end)
            (goto-char realend)
            (skip-chars-backward " \t\n")
            (when (not (= (point) realend))
              (setq delreg (buffer-substring-no-properties (point) realend))
              (kill-region (point) realend)
              (cond
               ((< (length delreg) 15)
                (setq delreg (maxima-replace-in-string "\n" " " delreg))
                (message (concat "\"" delreg "\" killed")))
               (t
                (setq delregbeg 
                      (maxima-replace-in-string "\n" " "(substring delreg 0 5)))
                (setq delregend
                      (maxima-replace-in-string "\n" " "(substring delreg -5)))
                (message (concat "\"" delregbeg " ... " delregend "\"  killed")))))
            (let ((ind (save-excursion
                         (goto-char realbeg)
                         (current-column)))
                  (here (point))
                  (there (make-marker)))
              (if (or
                   (string-match "\n" output)
                   (> (+ (current-column) (length output)) fill-column))
                  (progn
                    (insert "\n")
                    (setq here (point)))
                (insert " "))
              (insert (maxima-minor-output-mark) " " output 
                      (maxima-minor-output-mark-end))
              (set-marker there (point))
              (goto-char here)
              (goto-char (line-end-position))
;              (fill-region (line-beginning-position) (point))
              (if (string-match 
                   "\n" 
                   (buffer-substring-no-properties here (point)))
                  (forward-line -1)
                (forward-line 1))
              (indent-region (point) there ind)))
        (if (and arg twod)
            (let ((ind (save-excursion
                         (goto-char realbeg)
                         (current-column)))
                  (here))
              (save-excursion
                (goto-char realend)
                (insert (maxima-minor-output-mark) "\n")
                (setq here (point))
                (insert output (maxima-minor-output-mark-end))
                (indent-region here (point) ind))))))))

(defun maxima-minibuffer-on-line (&optional arg)
  "Send the current line to Maxima; display last output in minibuffer.
With an argument, insert \" ==> \" into the current buffer,
followed by the output.  In this case, anything in the line
after any occurrence of \" ==> \" will be deleted."
  (interactive "P")
  (maxima-minibuffer-on-region
   (maxima-line-beginning-position)
   (maxima-line-end-position)
   arg))

(defun maxima-minibuffer-on-form (&optional arg)
  "Send the current form to Maxima; display last output in minibuffer.
With an argument, insert \" ==> \" into the current buffer,
followed by the output."
  (interactive "P")
  (let ((beg (maxima-form-beginning-position))
        (end (maxima-form-end-position)))
    (save-excursion
      (when (re-search-backward "^[ \t]*$" beg t)
        (maxima-forward-over-comment-whitespace)
        (setq beg (point))))
    (maxima-minibuffer-on-region beg end arg)))

(defun maxima-minibuffer-on-determined-region (&optional arg)
  "Send a determined region to Maxima; display the output in the minibuffer.
The region is the region between `maxima-minor-prefix' and `maxima-minor-postfix'
With an argument, insert \" ==> \" into the current buffer,
followed by the output.  In this case, anything in the determined region
after any occurrence of \" ==> \" will be deleted."
  (interactive "P")
  (let ((beg)
        (end)
        (pt (point)))
    (save-excursion
      (if (re-search-backward maxima-minor-prefix nil t)
          (setq beg (match-end 0))
        (error "No beginning to determined region"))
      (goto-char pt)
      (if (re-search-forward maxima-minor-prefix nil t)
          (setq end (match-beginning 0))))
    (maxima-minibuffer-on-region beg end arg)))

(defun maxima-insert-last-output ()
  (interactive)
  (maxima-single-string-wait
            "block(emacsdisplay:display2d,display2d:false,linenum:linenum-1,%);")
  (let ((output (maxima-last-output-noprompt)))
    (maxima-single-string "block(display2d:emacsdisplay,linenum:linenum-1,%);")
    (insert (maxima-remove-whitespace-from-ends output))))

(defun maxima-insert-last-output-tex ()
  (interactive)
  (maxima-single-string-wait "tex(%);")
  (let ((output (substring (maxima-last-output-tex-noprompt) 2 -3)))
    (maxima-single-string "block(linenum:linenum-2,%th(2));")
    (insert output)))

;;; The Maxima minor mode

(defvar maxima-minor-mode-map nil
  "The keymap for maxima-minor-mode.")

(if maxima-minor-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c=m" 'maxima-minibuffer)
    (define-key map "\C-c=e" 'maxima-minibuffer-on-determined-region)
    (define-key map "\C-c=l" 'maxima-minibuffer-on-line)
    (define-key map "\C-c=r" 'maxima-minibuffer-on-region)
    (define-key map "\C-c=f" 'maxima-minibuffer-on-form)
    (define-key map "\C-c=o" 'maxima-insert-last-output)
    (define-key map "\C-c=t" 'maxima-insert-last-output-tex)
    (define-key map "\C-c=d" maxima-help-map)
    (setq maxima-minor-mode-map map)))

(unless (fboundp 'define-minor-mode)
  (defalias 'define-minor-mode 'easy-mmode-define-minor-mode))

(define-minor-mode maxima-minor-mode
  "Toggle Maxima minor mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Maxima minor mode is enabled, the following keystrokes
are in effect:
\\[maxima-minibuffer-on-determined-region]
   Send the region between the customizable regexps 
  `maxima-minor-prefix' and  `maxima-minor-postfix' to Maxima 
  and display the result in the minibuffer.
\\[maxima-minibuffer-on-line]
  Send the current line to Maxima and display the result in the minibuffer.
\\[maxima-minibuffer-on-region]
  Send the current region to Maxima and display the result in the minibuffer.
\\[maxima-minibuffer-on-form]
  Send the current form to Maxima and display the result in the minibuffer.
  (The form is the region between the preceding ; or $ and the subsequent
  ; or $)
With a prefix, the above commands will insert the output in the current
buffer, preceded by \" ==> \" (customizable with `maxima-minor-output').
\\[maxima-minibuffer]
  Prompt for an expression in the minibuffer, return result in minibuffer.
\\[maxima-insert-last-output]
  Insert the last Maxima result into the current buffer.
\\[maxima-insert-last-output-tex]
  Insert the last Maxima result in TeX form into the current buffer."
  ;; The initial value.;  :initial-value
  nil
  ;; The indicator for the mode line.;  :lighter 
  " maxima"
  nil)

;;; For highlighting the region being sent

(defun maxima-minor-mode-add-highlight ()
  (maxima-minor-mode-remove-highlight)
  (when (and maxima-minor-mode-region-begin 
             maxima-minor-mode-region-end)
    (if maxima-running-xemacs
        (progn
          (setq maxima-minor-mode-highlight
                (make-extent 
                 maxima-minor-mode-region-begin
                 maxima-minor-mode-region-end))
          (set-extent-property maxima-minor-mode-highlight 'face 'highlight))
      (setq maxima-minor-mode-highlight
            (make-overlay
             maxima-minor-mode-region-begin
             maxima-minor-mode-region-end))
      (overlay-put maxima-minor-mode-highlight 'face 'highlight))
    (setq maxima-minor-mode-region-begin nil)
    (setq maxima-minor-mode-region-end nil)))

(defun maxima-minor-mode-remove-highlight ()
  (when maxima-minor-mode-highlight
    (if maxima-running-xemacs
        (delete-extent maxima-minor-mode-highlight)
      (delete-overlay maxima-minor-mode-highlight))
    (setq maxima-minor-mode-highlight nil)))

(defun maxima-minor-mode-add-remove-highlight ()
  (if (or
       (eq this-command 'maxima-minibuffer-on-region)
       (eq this-command 'maxima-minibuffer-on-determined-region)
       (eq this-command 'maxima-minibuffer-on-form)
       (eq this-command 'maxima-minibuffer-on-line))
      (maxima-minor-mode-add-highlight)
    (maxima-minor-mode-remove-highlight)))

(defun maxima-minor-mode-highlighting ()
  (if maxima-running-xemacs
      (add-local-hook 'post-command-hook
                      'maxima-minor-mode-add-remove-highlight)
    (add-hook 'post-command-hook
              'maxima-minor-mode-add-remove-highlight nil t)))

(add-hook 'maxima-minor-mode-hook
          'maxima-minor-mode-highlighting)

(provide 'maxima)
;;; maxima.el ends here
