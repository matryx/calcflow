;;;;;;;;;;;;;;;;;; bookmode.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmode for emacs for interacting with various programs such as
;;; maxima, dfplot, xplot, shells, octave, maple Regions of text can be
;;; made sensitive, and clicking on these regions can run commands which
;;; will then possibly modify the buffer or bring up a display or bring
;;; in other files.  The input for the commands is edited, killed yanked
;;; etc, as if this were a normal buffer.  It also allows hypertext
;;; links, using the push-file ;;; Copyright William F. Schelter
;;;
;; This file is part of GNU Emacs and is covered by the Gnu GPL:
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following is a "simple shell" much like the one in version 18
;; of emacs.   Unfortunately cmint breaks most code which tries to use
;; the shell mode, and is rather complex.  
;;
(require 'sshell)
;;
;; Bugfix, default.el also contains this line. 
(setq auto-mode-alist (cons '( "\\.bk$" . book-mode) auto-mode-alist))
;;

(defvar book-faces nil)
(defvar book-face-default-background "pink")
(defvar book-face-default-foreground "white")
(defvar under-x-windows (eq (framep (selected-frame)) 'x))

(defun def-book-face (name eval-fun &optional copy-face bg fg)
  (make-face name)
  (put name 'book-eval-fun eval-fun)
  (or (member name book-faces)
      (setq book-faces (cons name book-faces)))
  (if copy-face (copy-face copy-face name))
  (cond ((and 
	  (eq (framep (selected-frame)) 'x)
	  (x-display-color-p))
	 (set-face-background name (or bg book-face-default-background))
	 (set-face-foreground name (or fg book-face-default-foreground)))
	((or bg fg) (invert-face name))))


(def-book-face 'book-result nil 'bold "blue" "white")
(def-book-face 'book-modified-result nil 'default "pink" "white")
(def-book-face 'book-mouse-face nil 'underline "black" "white")
(def-book-face 'book-mouse-face nil 'underline "white" "blue")
(def-book-face 'book-mouse-face nil 'underline "white" "blue")

(defun show-saved-properties (&optional pos)
  "Show properties at point which will be saved"
  (interactive "d")
  (let ((lis saved-properties) tem (ans "Props: "))
    (while lis
      (cond ((setq tem (get-text-property pos (car (car lis))))
	     (setq ans (format "%s (%s %s)" ans (car (car lis)) tem))))
      (setq lis (cdr lis)))
  (message "%s" ans)
  ans
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; set up menu bar on top, to allow popping file.

(defvar bookmode-menu-bar-book-menu (make-sparse-keymap "Book"))

(define-key bookmode-menu-bar-book-menu [kill-emacs] '("Exit No Saving!" . kill-emacs))
(define-key bookmode-menu-bar-book-menu [exit-emacs] '("Exit Emacs" . book-save-buffers-kill-emacs))



(define-key bookmode-menu-bar-book-menu [separator-xx]  '("--"))
(define-key bookmode-menu-bar-book-menu [bk-hardcopy] '("Print" . bk-hardcopy))
(define-key bookmode-menu-bar-book-menu [save-in-home] '("Save to Home" . save-in-home))
(define-key bookmode-menu-bar-book-menu [pop-find-file] '("Back" . pop-find-file))

(put 'pop-find-file 'menu-enable 'find-file-pushed)





(define-key menu-bar-file-menu [pop-find-file] '("Back to previous file" . pop-find-file))

(defun book-save-buffers-kill-emacs ()
  (interactive)
  (offer-to-save-books)
  (kill-emacs))

;;;


(defvar book-mode-map nil "Keymap for book mode" )
(defvar properties-to-save '(face book-command-arg read-only))
(defun book-mode ()
 "
Book mode provides commands for making certain regions sensitive
and putting commands on these regions.

The special keys or clicks in this mode are
\\<book-mode-map>
\\{book-mode-map}

Use \\[book-eval] or equivalently \\[book-mouse-eval] to run a command
associated to a region.  Such regions are distinguished by a different
face:  underlining, inverse video or a different font depending on
the screen capabilities.   Some such commands modify a result field
which is further in the buffer.   You may modify the command field
to try different parameters etc, and then reexecute.

\\[show-saved-properties] shows what commands are associated to
the current point.

Creating book files:
===================
After bringing in a new file in book mode (possibly by using
find file for a file with the .bk suffix, after making sure
bookmode.el has been loaded),
 use \\[book-mark-for-shell-eval] to make a region sensitive 
 for \\[book-mouse-eval].  This would also prompt for the shell 
 command you wish to run when that region is clicked on,
 use \\[book-mark-for-maxima-eval] to mark a region for evaluation 
 by maxima  or
 use \\[book-mark-for-maple-eval] to mark a region for evaluation 
 by maple.

To mark a region with other faces such as dfplot-eval use
\\[set-face-region].

If you edit a book-mode file without bringing it in bookmode, or
in another editor, you may edit the fields up to the end of the initial
s expression (i.e. up to the \page character), in order to change the filenames
or other material.   You may not edit the material after that \page, however
since the numbering scheme for tracking regions starts at that point, and
so editing after it would mean all offsets would likely be incorrect.

"  
  (interactive)
  (cond (buffer-read-only
	 (toggle-read-only 0)
	 (auto-save-mode 0)))
  (make-local-variable 'write-region-annotate-functions)
  (or (member 'book-write-region-annotate write-region-annotate-functions)
      (setq write-region-annotate-functions
	    (cons 'book-write-region-annotate
		  write-region-annotate-functions)))
  (setq under-x-windows (eq (framep (selected-frame)) 'x))
  (setq major-mode 'book-mode)
  (setq mode-name "Book")
  (or (boundp 'saved-properties)
      (setq saved-properties 
	    '((face) (book-command-arg) (read-only) )))
  (let ((lis book-faces) f)
	(while lis
	  (setq f (car lis))(setq lis (cdr lis))
	  (cond ((eq (framep (selected-frame)) 'x)
		 (cond ((get f 'book-eval-fun)
			(or (face-differs-from-default-p f)
			    (copy-face 'bold-italic f))
			(or (face-differs-from-default-p f)
			    (set-face-underline-p f t)))
		       (t
			(or (face-differs-from-default-p f)
			    (copy-face 'bold f))))))
	  (or (face-differs-from-default-p f)
	      (invert-face f))))
  (if book-mode-map
    nil
  (setq book-mode-map (make-keymap))
  (let ((i ?\ ))
    (while (<= i ?~)
      (define-key book-mode-map (make-string 1 i) 'book-self-insert)
      (setq i (+ i 1))))
  (define-key book-mode-map "\C-d" 'book-delete-char)

  (define-key book-mode-map [mouse-3] 'book-mouse-eval)
  (define-key book-mode-map [double-down-mouse-1] 'book-mouse-eval)
  (define-key book-mode-map [double-mouse-1] 'book-mouse-eval)
  
  
  (define-key book-mode-map "\C-cm" 'book-mark-for-maxima-eval)
  (define-key book-mode-map "\C-cu" 'book-unmark-all)
  (define-key book-mode-map "\C-cr" 'book-insert-sample-result)
  (define-key book-mode-map "\C-cs" 'book-mark-for-shell-eval)
  (define-key book-mode-map "\C-cl" 'book-mark-for-elisp-eval)

  (define-key book-mode-map "\C-cf" 'set-face-region)

  ;; hack
  (define-key book-mode-map "\C-cp" 'book-mark-for-maple-eval)
  (define-key book-mode-map "\C-cg" 'book-mark-for-gp-eval)
  (define-key book-mode-map "\C-c\C-cs" 'book-mark-for-Splus-eval)
  (define-key book-mode-map "\C-c\C-cr" 'book-mark-read-only)
  (define-key book-mode-map "\C-ca" 'book-mark-for-mma-eval)
  ;;

  (define-key book-mode-map "\C-ce" 'book-eval)
  (define-key book-mode-map "\C-c?" 'show-saved-properties)
  (define-key book-mode-map [menu-bar book] (cons "Book" bookmode-menu-bar-book-menu))
   )
  (use-local-map book-mode-map)
  ;; 30 xterminals beep randomly can really be anoying!
 ;  (setq visible-bell t)
  (setq trim-versions-without-asking t)
  )

;;;
;;; hack. It is extremly easy to get clicked twice on
;;; an expression. This little hack record down the time
;;; of the last mouse-eval and ignore the current click if
;;; it is less than  time-between-mouse-evals  apart, the 
;;;; default is 3 seconds.
;;;;        

;; I have removed this!   For 2 days i thought the mode was broken,
;; because it was doing nothing when I clicked... I guess I click too fast!
;; I have added a message to reinforce the idea that something is happening
;; when you click, to prevent double clicking..
;(defvar last-mouse-eval-time 0 "time of the last mouse-eval")
;(defvar time-between-mouse-evals 3)

;(defun book-mouse-eval (click)
;  "\\<book-mode-map>Follow a node reference near point.
;At end of the node's text, moves to the next node, or up if none."
;  (interactive "e")
;  (message "%s:%d" (car click)
;	 (- (nth 1 (current-time)) last-mouse-eval-time))
;  (cond ((member (car click) '(double-mouse-1 mouse-3))
;	 (let* ((start (event-start click))
;		(window (car start))
;		(pos (car (cdr start))))
;	   (select-window window)
;	   (goto-char pos)) 
;	 (let (time)
;	   (setq time (nth 1 (current-time)))
;	   (cond ((> (abs (- time last-mouse-eval-time))
;		     time-between-mouse-evals)
;		  (setq last-mouse-eval-time time)
;		  (book-eval))
;		 (t (message "you click too fast for mzou")))))))

(defun book-mouse-eval (click)
  "\\<book-mode-map>Follow a node reference near point.
At end of the node's text, moves to the next node, or up if none."
  (interactive "e")
;  (message "%s" click)
  (cond ((member (car click) '(double-mouse-1 mouse-3))
	 (let* ((start (event-start click))
		(window (car start))
		(pos (car (cdr start))))
	   (select-window window)
	   (goto-char pos))
	 (book-eval))))


(defun count-expr (ch string)
  (let ((n 0) (beg -1))
    (while (setq beg (string-match ch string (+ beg 1)))
      (setq n (+ n 1)))
    n))

(defun book-result-next (pos)
  "If next face change after pos is to book-result, return point"
  (let ((p (next-single-property-change pos 'face)))
    (and p (member (get-text-property p 'face) '(book-result
						 book-modified-result
						 ))
	 p)))

(defun book-eval ()
  "Try to eval the current expression as delimited by the special
characters"
  (interactive)
  (let* ((type (get-text-property (point) 'face))
	 (eval-fun (get type 'book-eval-fun))
	 )
    (or eval-fun (error "No book-eval-fun for type %s" type))
    (message "Using %s" eval-fun)
    (save-excursion
      (let* ((beg (or (previous-single-property-change (point) 'face) 1))
	     (end (or (next-single-property-change (point) 'face)
		      (point-max)))
	     (result (funcall eval-fun beg end type )))
	(cond (result
	       (save-excursion
		 (goto-char end)
		   (let ((p (book-result-next end)))
		     (or p
			 (error "No place to put result: %s" result))
		   (setq result (maxima-trim-result result))
		   (goto-char p)
		   (delete-region p (next-single-property-change p 'face))
		   (cond ((and (string-match "\n" result)
			       (not  (equal (current-column) 0)))
			  (insert "\n")))
		   (insert result)
		   (put-text-property p (point)  'face 'book-result)))))))) )

(defun book-insert-sample-result()
  "Insert a place holder for a result from previous expression"
  (interactive)
  (let ((beg (point)) ans)
    (insert   "RESULT ")
    (put-text-property beg (- (point) 1) 'face 'book-modified-result)
    (show-saved-properties beg)
    ))

;;;
;;;  hack.
;;;
(defun book-unmark-all (&optional remove-all pos)
  (interactive "P\nd")
  " Remove marks on regions that contains the current point. If a numeric 
    argument is given, it removes the read-only property also"
  (let ((inhibit-read-only remove-all))
    (book-unmark-expr pos)))

;;;
;;;
(defun book-unmark-expr (&optional pos)
  (interactive "d")
  "Remove special marks on regions that contain the current point.
   cannot remove the read-only property though. Use book-unmark-all
   to remove the read-only property"
  (let ((lis saved-properties) prop
	(inhibit-read-only t))
    (while lis 
      (setq prop (car (car lis))) (setq lis (cdr lis))
      (if (get-text-property pos prop )
	  (remove-text-properties
	   (or (previous-single-property-change pos prop)
	       (point-min))
	   (or (next-single-property-change pos prop)
	       (point-max))
	   (list prop) ) ))))
 
(defun add-to-buffer (buf str)
  (save-excursion
    (set-buffer buf)
    (goto-char (point-max))
    (insert str)))

(defun alter-face-at (p value)
  (let ((beg (previous-single-property-change (+ p 1) 'face))
	(end (next-single-property-change p 'face)))
    ;(message "%s" (list p beg end))
    (put-text-property beg (or end (point-max)) 'face value)
    ))
	
(defun book-self-insert (&optional arg)
  "Change a result font to indicate the corresponding command was altered."
  (interactive "p")
  (maybe-change-result-field)
   (self-insert-command arg))

(defun maybe-change-result-field ()
  (let* ((p (point)) 
	 (prop (and (> p 1) (get-text-property (- p 1) 'face))))
    (cond ((get prop 'insert)
	   (setq p (book-result-next (next-single-property-change
				      (- p 1) 'face)))
	   (and p
		(alter-face-at p 'book-modified-result))))))

(defun book-delete-char (n &optional killflag)
  (interactive "p\nP")
  (maybe-change-result-field)
  (delete-char n killflag))



;;;;;;;;; buffer property saving for a file.
  
(defun buffer-properties-prop (min max prop)
  "Go thru buffer finding changes in value of PROP text property, and
return a list of beg1 end1 value1 beg2 end2 value2 ...  for text
values of PROP"
  
  ;;
  ;; Bug fix, the original version does work when the the char
  ;; at MIN or MAX have some special marks.   7-24-95, mzou
  ;;                                      
  ;; should write a better version!
  ;; 
  (let ((p min) beg end beginning ans alist val tem)
    ;; check to see if MIN has non-nil mark
    (and p (setq beginning (get-text-property p prop))) 
    (while (and p (or (setq beg (next-single-property-change  p prop))
		      beginning))   ; the whole buffer may be marked
				    ; read-only. 
				    ;
      (cond (beginning              ; if there are marks at MIN
	     (setq beg min)         ; save it first.
	     (setq beginning nil))) ; 
      (or (number-or-marker-p beg)  ; there are cases when beg is nil,
	  (setq beg max))           ; and it broke there. ???
      (cond ((>= beg max)           ; 
	     (setq beg max)))       ; if called on a region ...

      (setq end (next-single-property-change beg prop))
      (or (number-or-marker-p end)  ;
	  (setq end max))           ; bug fix

      (cond ((>= end max)
	     (setq end max)
	     (setq p nil)) 
            ((get-text-property end prop)
	     (setq p (- end 1)))
	    (t (setq p end)))
      (cond ( (setq val (get-text-property beg prop)) ; save non-nil only 
	      (or (setq tem (assoc val ans))
		  (setq ans (cons (setq tem (list val)) ans)))
	      (setq tem (nconc tem (list beg end)))))
      )
    ans))


(or (member 'install-props-after-insert-file after-insert-file-functions)
       (setq after-insert-file-functions
	    (cons 'install-props-after-insert-file
		  after-insert-file-functions)))

(defvar install-props-magic "\n"
  "A regexp such that (looking-at install-props-magic) is t
and  going (match-end 0) will move us to the beginning of the
saved-properties list to install.  The saved-properties list is
followed by a new page character, and then the regular text of the
file")
 
(defun install-props-after-insert-file (n)
    (cond
     ((looking-at install-props-magic)
      (install-props-after-insert-file1 n))
     (t n)))

;;;
;;; hack, insert mouse-face property on theose regions
;;; which are suppose to be executed when a click event
;;; happens on them.  These regions are marked with one 
;;; of the following faces.
;;;
(setq put-mouse-face-on-them nil)
(defvar put-mouse-face-on-them
  (list 'book-shell-eval 'book-elisp-eval 'dfplot-eval 
	'maxima-eval-insert 'maxima-eval 'octave-eval
	'xplot-eval 'shell-eval-region 'maple-eval
	'gp-eval 'Splus-eval 'book-shell-eval-insert 
	'maple-eval-insert 'gp-eval-insert 'mma-eval 
	'mma-eval-insert 'Splus-eval-insert))
;;;
;;;
(defun install-props-after-insert-file1 (n)
  (let* (val (pt (point)) (mod (buffer-modified-p)))
    (goto-char (match-end 0))
    (let ((saved-properties (read (current-buffer)))
	  (end (point)))
      (or (looking-at "") (error "bad props"))
      (forward-char 1)			;past new page mark.
      (delete-region pt (point))
      (let ((lis saved-properties)
	    prop x values)
	(while lis
	  (setq x (car lis))
	  (setq lis (cdr lis))
	  (setq prop (car x))
	  (setq values (cdr x))
	  (while values
	    (setq x (car values))
	    (setq values (cdr values))
	    (setq val (car x))
	    (setq x (cdr x))
	    (while x
	      (put-text-property (car x) (nth 1 x)  prop val)
	      ;;
	      ;; hack 
	      (cond ( (member val put-mouse-face-on-them )
		      (put-text-property (car x) (nth 1 x)
					 'mouse-face 'book-mouse-face)))
	      ;; 
	      (setq x (nthcdr 2 x))))
	  ))
      (or mod (set-buffer-modified-p nil))
      (- n (- end pt))
      )))




;; unfortunately format truncates at newlines...
;; format "%s%S" install-props-magic ans
(defun book-write-region-annotate (beg end)
  (save-excursion
    (let (ans prop (lis properties-to-save) vals string)
      (while lis
	(setq prop (car lis)) (setq lis (cdr lis))
	(setq vals  (buffer-properties-prop beg end prop))
	(cond (vals
	       (setq ans (cons (cons prop vals)
			       
			       ans))))
	)
      (cond (ans
	     (setq ans (nreverse ans))
	     (let ((buf (generate-new-buffer " saving")))
	       (set-buffer buf)
	       (insert install-props-magic)
	       (prin1 ans buf)
	       (insert "")
	       (setq string (buffer-substring (point-min) (point-max)))
	       (kill-buffer buf)))
	    (t  (setq string (format "%s%S" install-props-magic ans ))))
      (list (cons 1 string)))))


(defun set-face-region (&optional face)
  "Make the current region have FACE, eg dfplot-eval, octave-eval"
  (interactive)
  (or face (setq face (completing-read "Face: "
				       (apply 'vector (face-list)))))
  (cond ((stringp face) (setq face (intern face))))
  (put-text-property (region-beginning) (region-end) 'face face)
  (put-text-property (region-beginning) 
		     (region-end) 
		     'mouse-face 'book-mouse-face)
  )

;;;;;;;; code for evaluation of general form in shell ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-book-face 'book-shell-eval 'book-shell-eval 'bold "YellowGreen" "black")
(def-book-face 'book-elisp-eval 'book-elisp-eval 'bold  "YellowGreen" "red2")
(def-book-face 'book-elisp-eval 'book-elisp-eval 'bold  "White" "red2")

(defvar book-command-arg-history nil)

(defun book-mark-for-shell-eval (&optional do-insert beg end com1)
  "Mark the region for evaluation by shell.  You must quote spaces
  with control-q, because of the completion mechanism.  If a numeric
  argument is set then the next <Result> place will get the output
  from running the shell command.  If 'insert' mode is specified then
  the emacs will wait until the command completes, whereas otherwise
  it will run in the background." 

  (interactive "P\nr")
  (let ((com (or
	      com1 (get-text-property beg 'book-command-arg))) 
	(table
	 (buffer-properties-prop (point-min) (point-max) 'book-command-arg)))
    (book-unmark-expr beg) 
    (put-text-property beg end 'face 
		       (if do-insert 'book-shell-eval-insert 'book-shell-eval
			   ))
    (put-text-property beg
		       end
		       'book-command-arg
		       (or com1 
			   (completing-read "Shell Command: " table nil nil
					    com  'book-command-arg-history
					    )))
    (or (not do-insert) (maybe-add-result-field end))
    ))

(defun maybe-add-result-field (end)
  (or
   (not book-maxima-auto-result-insert)
      (book-result-next end)
      (save-excursion (goto-char end)
		      (insert
		       (nth (random (length book-maxima-auto-result-insert))
			    book-maxima-auto-result-insert))
		      (book-insert-sample-result))))


(defun book-mark-for-elisp-eval (&optional beg end com1)
  "Mark for elisp eval.   You must quote spaces with control-q, because
   of the completion mechanism."
  (interactive "r")
  (let ((com (or com1 (get-text-property beg
					 'book-command-arg)))
	(table (buffer-properties-prop (point-min) (point-max)
				       'book-command-arg))
	)
    (book-unmark-expr beg)
    (put-text-property beg
		       end
		       'face 'book-elisp-eval)
    (put-text-property beg
		       end
		       'mouse-face 'book-mouse-face)
    (put-text-property beg
		       end
		       'book-command-arg
		       (or com1 
			   (completing-read "Elisp Command: " table nil nil
					    com  'book-command-arg-history
					    )))))
(defun book-elisp-eval (beg end type &optional command)
  (let ((com (or command (get-text-property beg 'book-command-arg))))
    (eval (read com)))
  nil)

;;;
;;; mark a region to be read-only. This is primarily for
;;; buttons in the buffer. (don't want students  midify them)
;;;  7-24-95, mzou
;;;
(defun book-mark-read-only (&optional beg end)
  "Mark the current region read-only. To remove read-only property,
    use the function book-unmark-expr"
  (interactive "r")
  (put-text-property beg end 'read-only t)
  (message "region [%d %d] marked read-only" beg end))
;;;
;;;

(defvar find-file-pushed nil "List of file positions from find-file-pushed")
(defun push-find-file (name &optional string)
  "Follow a link to FILENAME optionally searching for STRING in the file"
  (interactive)
  (setq  find-file-pushed
	 (cons (make-marker )  find-file-pushed))
  (set-marker (car find-file-pushed) (point) (current-buffer))
  (find-file name)
  (cond (string
	 (let ((at (point)))
	   (goto-char (point-min))
	   (or (search-forward string nil t)
	       (goto-char at)))))
  )
(defun pop-find-file ()
  "If you have followed a link, return back to where you were"
  (interactive)
  (cond (find-file-pushed
	 (let ((at (car find-file-pushed)))
	   (switch-to-buffer (marker-buffer at))
	   (goto-char at)
	   (setq find-file-pushed (cdr find-file-pushed))
	   (set-marker at nil))))
  nil)

;;;;;;;;;;;Postscript insertion stuff;;;;;;;;;;;;;;
(def-book-face 'book-postscript-insert 'book-postscript-insert-eval nil
  "beige" "black")
(defvar book-faces-that-make-postscript '((dfplot-eval "~/dfplot.ps")
					  (xplot-eval "~/zplot.ps")
					  (maxima-eval "~/maxout.ps")
					  (octave-eval "~/gnuplot.ps")
					  (maxima-eval-insert "~/maxout.ps")
					  ))


(defun book-postscript-insert-eval (beg end type)
  (let* ((com (get-text-property beg 'book-command-arg))
	 (p (previous-single-property-change beg 'face))
	 (tem (and p (assoc (get-text-property (- p 1) 'face)
			    book-faces-that-make-postscript)))
	 (menu 
		(list
  		 "Do What1?"
		 (list "Insert Named Postscript File" 'book-set-postscript-value
		       beg end)
		 (and (car com)
		      (list
		       "View Current Postscript" 'book-view-postscript   (car com)))
		 (and tem
		      (list (concat "Set Postscript to "(nth 1 tem))
			    'book-set-postscript-value beg end (nth 1 tem)))
		
		  (and tem
		       (list (concat "View "(nth 1 tem))
			     'call-process  "ghostview" nil nil nil
			     (expand-file-name(nth 1 tem))
			     ))
		  (list "Cancel")
		  )))
	 
    (setq menu (delete nil menu))
    (setq com (x-popup-menu t (list "Do whate? " menu)))
    (message "%s" com)
    (eval com)
    nil
    ))

(defun book-view-postscript (string)
  (let ((buf (get-buffer-create "ps view")))
    (set-buffer buf)
    (erase-buffer)
    (insert string)
    (call-process-region (point-min) (point-max) "sh" t
			 0 ; means dont wait.
			 nil "-c"
			 ;; construct command to pass to the shell.
			 (concat
			  (cond ((looking-at "%PS") "")
				(t "gzip -dc | "))
			  "ghostview -")
			 
	)))
	 
(defun book-set-postscript-value ( beg end &optional file)
  (interactive "r")
  "Put the postscript FILE as a file to insert for current region"
  (or file (setq file (read-file-name "Postscript file: " )))
  (let* ((date 		 (nth 5 (file-attributes file)))
         (buf (generate-new-buffer "pszip"))
	 string)
    (save-excursion
      (set-buffer buf)
      (insert-file-contents file nil)
      (call-process-region  (point-min)(point-max) "gzip" t buf nil "-c")
      (setq string (buffer-substring (point-min) (point-max)))
      (kill-buffer buf))
    (put-text-property beg end 'book-command-arg
		       (list string date))))

;;;;;;;;;;end postscript insert stuff;;;;;;;;;;

(defvar book-shell-program nil
 "Program to use for shell for executing commands given to book-shell-eval
`sh' will be used if none is supplied")

(defun book-start-process (name buffer program &rest prog-args)
  "Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER PROGRAM &rest PROGRAM-ARGS
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
"
  (let ((proc (apply 'start-process name buffer program prog-args))
	(i 0)
	result
	tem
	buf)
    (put-process-prop proc 'last-output "")
    (setq buf (get-buffer buffer))
    (set-marker (process-mark proc)
		(if buf (save-excursion (set-buffer buf) (point-max)) 1)
		(set-process-buffer proc (or buf (get-buffer-create buffer))))
    (put-process-prop proc 'started nil)
    (set-process-filter proc 'book-process-filter)
    (while (< i 10)
      (cond ((get-process-prop proc 'started)
	     (setq i 11)
	     (setq result proc))
	    (t (setq i (+ i 1))
	       (sleep-for 1))))
    (or result (error "could not start process %s" name))
    result))


(defun book-shell-eval (beg end type &optional command)
  (let* (res
	 (com (or command (get-text-property beg 'book-command-arg)))
	 (sh (or book-shell-program
				       "/bin/sh"))
	 (proc (start-process "*book-shell-output*" "*book-out*"
				   sh
				   "-s"
				   ))
	 (buf (process-buffer proc))
	 (marker (process-mark proc))
	 (at-end "<AT fayve END>")
	 )
    (let ((i 10))
      (while (> i 0)
	(cond ((setq beg (marker-position marker))
	       (setq i -1)))
	(setq i (- i 1))
	(sit-for 0 400)
	))
    (or beg (error "problem starting process ?"))
    (cond (com
	   (message "executing in %s: %s" sh com)
	   (process-send-string proc
				(concat com
					";echo '" at-end
					"'\nexit\nexit\n\nn"))
	   ))
    (cond ((eq type 'book-shell-eval-insert)
	   ;; must grab the result...
	   (while (equal (process-status proc) 'run)
	     (sleep-for  1))
	   (save-excursion
	     (set-buffer buf)
	     (goto-char beg)
	     (cond ((search-forward at-end nil t)
		    (message "..done")
		    (buffer-substring beg (- (point) (length at-end) 1)))
		   (t (error "did not terminate normally")))))
	  (t nil))))

(def-book-face 'book-shell-eval-insert 'book-shell-eval
      'underline "YellowGreen" "black")
(put 'book-shell-eval-insert 'insert t)

(defun dfplot-eval (beg end type)
  "Call dfplot on a region and send output to ~/dfplot.ps"
  
  (let* ((default-directory "~/")
	 (proc (book-start-process "*book-dfplot-output*" "*book-out*"
			     "dfplot")))
    (let ((com 	(concat (buffer-substring beg end)
			" ;\n plot ; \n set output 'dfplot.ps' ;\n "
			" replot \n\n quit \n"))
	  )
      (message "executing %s" com)
      (process-send-string proc com))))

(def-book-face  'dfplot-eval 'dfplot-eval 'underline "yellow2"  "black")

;;;;;;;; code for maxima evaluation.;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make maxima-eval-insert face do insertion of result

(def-book-face 'maxima-eval-insert 'maxima-eval 'underline "yellow" "black" )

(put 'maxima-eval-insert 'insert t)

(def-book-face 'maxima-eval 'maxima-eval 'underline "yellow" "black" )

(defvar maxima-eval nil)

(defun book-mark-for-maxima-eval (eval-only beg end)
  "Mark the current region to be evaluated by maxima and
substituted in the next book result region.   If a
numeric arg is supplied, dont wait for the evaluation nor
insert the result.  The variable book-maxima-auto-result-insert
affects whether a sample result is inserted.
"
  (interactive "P\nr")
  (put-text-property beg
		     end 
		     'face (if eval-only 'maxima-eval 'maxima-eval-insert))
  (put-text-property beg
		     end
		     'mouse-face 'book-mouse-face)
  (or eval-only
      (not book-maxima-auto-result-insert)
      (book-result-next end)
      (save-excursion (goto-char end)
		      (insert
		       (nth (random (length book-maxima-auto-result-insert))
			    book-maxima-auto-result-insert))
		      (book-insert-sample-result)))
  (show-saved-properties beg)
  )

(defun m1 (&optional eval-only )
  (interactive "P")
  (let (beg end)
  (save-excursion
    (progn (re-search-forward "[ \n\t]" nil t) (setq end (- (point) 1))))
  (save-excursion
    (progn (re-search-backward "[ \n\t]" nil t) (setq beg (+ 1 (point)))))
  (book-mark-for-maxima-eval eval-only beg end)))
  
(defvar book-maxima-auto-result-insert '(" yields " " evaluates to "
					 " returns " " produces " " gives " )
  "If not nil a sample result field will be inserted after the
maxima expression.   It should be a list of strings which will
be used at random between the form to eval and the `result'")

(defvar book-maxima-ready-for-input nil)


(defun add-to-process-buffer (proc str)
;  (setq me proc)
  (let (moving (buf (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point) (process-mark proc)))
	  (save-excursion
	    (goto-char (process-mark proc))
	    (insert str)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
      (set-buffer buf))))
(defun get-process-prop (proc prop)
  (if (processp proc) (setq proc (intern (process-name proc))))
  (get proc prop))

(defun put-process-prop (proc prop val)
  (if (processp proc) (setq proc (intern (process-name proc))))
  (put proc prop val))

(defvar last-maxima-result nil)
(defvar book-result nil)

(defun book-maxima-process-filter ( proc str)
  (let (tem )
    (book-process-filter proc str)
    (put-process-prop proc 'last-output
		      (concat (get-process-prop proc 'last-output)
			      str))
    (cond ((setq tem (string-match "(C[0-9]+)[ ]*$"
				   (get-process-prop proc 'last-output)))
	   (setq book-maxima-ready-for-input t)
	   (setq last-maxima-result
		 (substring (get-process-prop proc 'last-output)  0 tem ))
	   (put-process-prop proc 'last-output "")
	   )
	  ((string-match ">>$" str)
	   (process-send-string proc ":t\n")
	   (message "had error")
	   (setq book-result "<had-error>"))
	  )))


(defun book-process-filter ( proc str)
  (add-to-process-buffer proc str)
  (put-process-prop proc 'started t)
  )
    
(defun maxima-restart ()
  (setq maxima-eval nil)
  (if (get-buffer "*maxima-eval*")
      (kill-buffer (get-buffer "*maxima-eval*") )))

(defun book-maxima-interrupt ()
  "Interrupt the *maxima-eval* process running for book mode"
  (interactive)
  (cond (maxima-eval
	 (interrupt-process maxima-eval))
	(t (error "*maxima-eval* process not found"))))

(defun maxima-eval (beg end type)
  "Evaluate the region returning a result"
  (let (tem (process (get-process  "*maxima-eval*")))
    (cond ((not  (and maxima-eval
		      (setq process (get-buffer-process maxima-eval))))
	   (cond ((and under-x-windows x-display-name
		       (not (getenv "DISPLAY")))
		  (setq process-environment
			(cons (concat "DISPLAY=" x-display-name)
			      process-environment))))
	   (let ((default-directory "~/"))
	     (setq maxima-eval (make-sshell "maxima-eval" "maxima" )))
	   (setq process (get-buffer-process  maxima-eval))
	   (set-process-filter process 'book-maxima-process-filter)
	   ))
    (let ((com (buffer-substring beg end)))
      (or (string-match  "[;$][ \t\n]*$" com)
	  (setq com (concat com ";" )))
      (setq com (concat com "\n"))
      (while (not book-maxima-ready-for-input)
	(message "waiting till maxima ready for input..")
	(process-send-string process "\n")
	(sleep-for 1))
      (message  "sending command :%s " com )
      (setq last-maxima-result nil)
      (process-send-string process com))
    (cond ((equal type 'maxima-eval-insert)
	   (while (not last-maxima-result)
	     (message "waiting for result...")
	     (sleep-for 1))
	   (message "done")
	   (setq tem (maxima-trim-result last-maxima-result))
	   (setq last-maxima-result nil)
	   tem))))

(defun maxima-trim-result (x)
  (let (tem)
    (cond ((equal 1 (count-expr "\n" x))
	   (cond ((string-match "(D[0-9]+)" x)
		  (setq x (substring x (match-end 0)))))
	   (cond ((setq tem (string-match "\n$" x))
		  (setq x (substring x 0 tem))))
	   (cond ((not (string-match "\n" x))
		  (cond ((string-match "[ \t]+" x)
			 (setq x (substring x (match-end 0)))))))
	   x)
	  (t (cond ((setq tem (string-match "(D[0-9]+)" x))
		    (while (< tem (match-end 0))
		      (aset x tem ? )
		      (setq tem (+ tem 1)))))
	     x))))
;;;;;;;; end code for maxima evaluation.

;; for octave eval
(defun octave-eval (beg end type)
  "Call octave on a region and send output to ~/octave.ps"
  
  (let* ((default-directory "~/")
	 (proc (book-start-process "*book-octave-output*" "*book-out*"
			     "octave")))
    (let ((com 	(concat
	    "gnuplot_binary='tk_gnuplot1';\n"
	    "set title 'Plot for " (user-real-login-name)  " on "
                (current-time-string) "';\n"
		 (buffer-substring beg end)
			"\n quit;\n"
			))) 
      (message "executing %s" com)
      (process-send-string proc com))))

(def-book-face 'octave-eval 'octave-eval 'underline "yellow3" "black")
;; end octave


;; xplot 
(defun xplot-eval (beg end type)
  "Call xplot on a region and send output to ~/xplot.ps"
  
  (let* ((default-directory "~/")
	 (proc (book-start-process "*book-xplot-output*" "*book-out*"
			     "xplot")))
    (let ((com 	(concat (buffer-substring beg end)
			"\n plot \n  quit ; \n quit;\n\n")))
      (message "executing %s"  com)
      (process-send-string proc com))))

(def-book-face 'xplot-eval 'xplot-eval 'underline "yellow3" "black")
;; end xplot

(provide 'bookmode)

;;;
;;; additions from mzou adopting maxima to maple, and 
;;; cours- name stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;  mkdir ~/course-name if not already there
;;    copy the master file into it. These are
;;    done by the shell script find_course,
;;    push-find-file ~/course-name/master.bk
;;    set the global variable course-name (???)
;;
(defvar course-name nil)
(defvar project-name nil)
(defvar start-time nil)



(defun select-course (name)
   "Make a directory ~/name and copy the master file into it"
 (setq course-name  name)
 (push-find-file (concat name "/master.bk")))
; (call-process "select_course" nil nil nil name)
; (push-find-file (concat (getenv "HOME") "/" name "/master.bk"))
; (setq start-time (current-time-string))
; (message (concat (getenv "HOME") "/" name "/master.bk")))
 


;;
;;
;;  copy the project file into ~/course-name/ and
;;  push-find-file the-proj-file. 
;;
(defun select-project (name)
   "Select a project. Copy the proj-file into the right place"
   (setq project-name name)	
   (push-find-file name))	

;;;
;;;  convert buffer to latex and print a hardcopy 
;;;  if possible. have to write to a tmp-file in ~/
;;;  because the usr may not have the permission to
;;;  save the current buffer.
;;;
(defun bk-hardcopy ( )
  "print out a nice hardcopy of the current buffer"
  (interactive)
  (let ((tmp-file) (old-back make-backup-files))
    (setq tmp-file (concat (getenv "HOME") "/pj_.bk"))
    (setq make-backup-files nil)
    (write-file tmp-file) 
    (setq make-backup-files old-back)
    (start-process "printbk" nil "printbk" tmp-file )
    ;(call-process "printbk"  nil nil tmp-file "&")
    )
  nil)	
;;;
;;; eval the shell command in region
;;;
(def-book-face 'shell-eval-region 'shell-eval-region
  'bold "yellow2" "blue")
(defun shell-eval-region (beg end type)
  "Exec the shell command in region"
  (let* ((default-directory "~/")
	 (proc (book-start-process "*book-shell-output*" "*book-out*"
			     "sh")))
    (let ((com 	(concat (buffer-substring beg end)
			"\n exit \n")))
      (message "executing %s" com)
      (process-send-string proc com))))
;;;
;;;  A may be useful function. 
;;;
(defun turnin-project ( )
  "Turn in the current project"
  (save-buffer)
  (let ( project-file )
    (setq project-file (concat (getenv "HOME") "/"
				course-name "/"
				project-name))
    (call-process "turnin_project" nil nil nil
		  course-name project-file
		  start-time (current-time-string) ) ))

(defun offer-to-save-books ()
  (let ((tem  (buffer-list))
	vars b)
    (while tem
      (setq b (car tem))
      (setq tem (cdr tem))
      (setq vars (buffer-local-variables b))
      (cond ((and
	      (buffer-modified-p b)
	      (eq (cdr (assoc 'major-mode vars)) 'book-mode)
	      (y-or-n-p  (format "Save changes to %s as %s ?"
			 (buffer-name b)
			 (get-home-directory-name
			  (buffer-file-name b)))
			 ))
	     (save-excursion (set-buffer b)
			     (save-in-home)))))))

(defun get-home-directory-name (name)
  (let ((p (file-name-nondirectory name))
	(dir "~/"))
    (cond ((string-match "/books/\\|/courses/" name)
	   (setq f (substring name (match-beginning 0)))
	   (setq dir (concat "~" (file-name-directory f)))
	   (concat dir p))
	  (t name))))
	     
(defun save-in-home ()
  (interactive "")
  (let* ((name (buffer-file-name (current-buffer)))
	 (new (get-home-directory-name name))
	 (default-directory default-directory ))
    (make-directory (file-name-directory new) t)
    (write-file new)))
    		
	
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; code for maple evaluation.;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; make maple-eval-insert face do insertion of result
;;
(def-book-face 'maple-eval-insert 'maple-eval
  'underline "Greenyellow" "black" )

(put 'maple-eval-insert 'insert t)

(def-book-face 'maple-eval 'maple-eval 'underline "Greenyellow" "black" )

(defvar maple-eval nil)

(defun book-mark-for-maple-eval (eval-only beg end)
  "Mark the current region to be evaluated by maple and
substituted in the next book result region.   If a
numeric arg is supplied, dont wait for the evaluation nor
insert the result.  The variable book-maxima-auto-result-insert
affects whether a sample result is inserted.
"
  (interactive "P\nr")
  (put-text-property beg
		     end 
		     'face (if eval-only 'maple-eval 'maple-eval-insert))
  (put-text-property beg
		     end
		     'mouse-face 'book-mouse-face)
  (or eval-only
      (not book-maxima-auto-result-insert)
      (book-result-next end)
      (save-excursion (goto-char end)
		      (insert
		       (nth (random (length book-maxima-auto-result-insert))
			    book-maxima-auto-result-insert))
		      (book-insert-sample-result)))
  (show-saved-properties beg)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar book-maple-ready-for-input nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar last-maple-result nil)
(defun book-maple-process-filter ( proc str)
  (let (tem )
    (book-process-filter proc str)
    (put-process-prop proc 'last-output
		      (concat (get-process-prop proc 'last-output)
			      str))

    (cond ((setq tem (string-match ";#z#"  ;;; terminating symbol  
				   (get-process-prop proc 'last-output)))
	   (cond ((setq tem
			(string-match "^>>[ ]*$" ;;; the prompt
				      (get-process-prop proc 'last-output)))
		  (setq book-maple-ready-for-input t)
		  (setq last-maple-result (get-process-prop proc 'last-output))
		  (put-process-prop proc 'last-output ">> "))
		 )
	  )
    )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun maple-restart ()
  (setq maple-eval nil)
  (if (get-buffer "*maple-eval*")
      (kill-buffer (get-buffer "*maple-eval*") )))

(defun book-maple-interrupt ()
  "Interrupt the *maple-eval* process running for book mode"
  (interactive)
  (cond (maple-eval
	   (interrupt-process maple-eval))
	(t (error "*maple-eval* process not found"))))

(defun maple-eval (beg end type)
  "Evaluate the region returning a result"
  (let (tem (process (get-process  "*maple-eval*")))
    (cond ((not  (and maple-eval
		      (setq process (get-buffer-process maple-eval))))
	   (cond ((and under-x-windows x-display-name
		       (not (getenv "DISPLAY")))
		  (setq process-environment
			(cons (concat "DISPLAY=" x-display-name)
			      process-environment))))
	   (let ((default-directory "~/"))
	     (setq maple-eval (make-sshell "maple-eval" "maple52" )))
	   (setq process (get-buffer-process  maple-eval))
	   (set-process-filter process 'book-maple-process-filter)
	   (process-send-string process 
	   "interface(echo=0,plotdevice=x11,prompt=`>> `,screenwidth=80);gc(0);")
	   )
	  )
    (let ((com (buffer-substring beg end)))
      (setq com (concat com ";#z#\n"))
      (while (not book-maple-ready-for-input)
	(message "waiting till maple ready for input..")
	(process-send-string process ";#z#\n")
	(sleep-for 1))
      (message  "sending command :%s " com )
      (setq last-maple-result nil)
      (process-send-string process com)
      )
    (cond ((equal type 'maple-eval-insert)
	   (while (not last-maple-result)
	     (message "waiting for result...")
	     (sleep-for 1)
	     )
	   (message "done")
	   (setq tem (maple-trim-result  last-maple-result))
	   (setq last-maple-result nil)
	   tem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun maple-trim-result (str)
  (let ( (tem) (x) (bgn 0) (end -1) (tstr) (ll) )
    (setq x "") (setq tstr "")
    (setq ll (length str))
    ;;
    ;; str contains mixed inputs and outputs, with inputs
    ;; matchs "^>>[^\n]*". Strip out all inpus. Also, maple
    ;; insert an extra "\n" at both the beginning and the end 
    ;; of its outputs (except for error mesg)
    ;;
    (while (setq end (string-match "^>>[^\n]*" str (+ end 1)))
      (or (< end bgn)
          (setq x (concat  x (substring str bgn end))))
      (setq tem (+ (match-end 0) 1))
      (if  (> ll tem)
	  (setq tstr (substring str tem (+ tem 1))))
      ( cond  ( (string-equal tstr "\n") 
		(setq bgn  (+ tem 1)))
	      (t (setq bgn tem)))
      )
    ;;
    ;; if there is output at all, x is at least of length 2
    ;; including a trailling \n\n (yes 2 of them).
    ;; Strip one \n out.
    ;;
    (if (< (length x) 2) 
	(setq x "OK")
      (setq x (substring x 0 (- (length x ) 1))))
    ;;
    ;; from maxima-trim-result. Strip out spaces if
    ;; output fits in one line.
    ;;
    (cond ((equal 1 (count-expr "\n" x))
	   (cond ((setq tem (string-match "\n$" x))
		  (setq x (substring x 0 tem))))
	   (cond ((not (string-match "\n" x))
		  (cond ((string-match "[ \t]+" x)
			 (setq x (substring x (match-end 0)))))))
	   x)
	  (t x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; code for gp evaluation.;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; make maple-eval-insert face do insertion of result
;;
(def-book-face 'gp-eval-insert 'gp-eval 'underline "chartreuse" "black" )

(put 'gp-eval-insert 'insert t)

(def-book-face 'gp-eval 'gp-eval 'underline "chartreuse" "black" )

(defvar gp-eval nil)

(defun book-mark-for-gp-eval (eval-only beg end)
  "Mark the current region to be evaluated by gp and
substituted in the next book result region.   If a
numeric arg is supplied, dont wait for the evaluation nor
insert the result.  The variable book-maxima-auto-result-insert
affects whether a sample result is inserted.
"
  (interactive "P\nr")
  (put-text-property beg
		     end 
		     'face (if eval-only 'gp-eval 'gp-eval-insert))
  (put-text-property beg
		     end
		     'mouse-face 'book-mouse-face)
  (or eval-only
      (not book-maxima-auto-result-insert)
      (book-result-next end)
      (save-excursion (goto-char end)
		      (insert
		       (nth (random (length book-maxima-auto-result-insert))
			    book-maxima-auto-result-insert))
		      (book-insert-sample-result)))
  (show-saved-properties beg)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar book-gp-ready-for-input nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar last-gp-result nil)
(defun book-gp-process-filter ( proc str)
  (let (tem )
    (book-process-filter proc str)
    (put-process-prop proc 'last-output
		      (concat (get-process-prop proc 'last-output)
			      str))
    (cond ((setq tem (string-match "\?[ ]*$"
				   (get-process-prop proc 'last-output)))
	   (setq book-gp-ready-for-input t)
	   (setq last-gp-result
		 (substring (get-process-prop proc 'last-output)  0 tem ))
	   (put-process-prop proc 'last-output "")
	   )
	  )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gp-restart ()
  (setq gp-eval nil)
  (if (get-buffer "*gp-eval*")
      (kill-buffer (get-buffer "*gp-eval*") )))

(defun book-gp-interrupt ()
  "Interrupt the *gp-eval* process running for book mode"
  (interactive)
  (cond (gp-eval
	 (interrupt-process gp-eval))
	(t (error "*gp-eval* process not found"))))

(defun gp-eval (beg end type)
  "Evaluate the region returning a result"
  (let (tem (process (get-process  "*gp-eval*")))
    (cond ((not  (and gp-eval
		      (setq process (get-buffer-process gp-eval))))
	   (cond ((and under-x-windows x-display-name
		       (not (getenv "DISPLAY")))
		  (setq process-environment
			(cons (concat "DISPLAY=" x-display-name)
			      process-environment))))
	   (let ((default-directory "~/"))
	     (setq gp-eval (make-sshell "gp-eval" "gp" )))
	   (setq process (get-buffer-process  gp-eval))
	   (set-process-filter process 'book-gp-process-filter)
	   (process-send-string process  "\n")
	   )
	  )
    (let ((com (buffer-substring beg end)))
      (setq com (concat com "\n"))
      (while (not book-gp-ready-for-input)
	(message "waiting till gp ready for input..")
	(process-send-string process "\n")
	(sleep-for 1))
      (message  "sending command :%s " com )
      
      (setq last-gp-result nil)
      (process-send-string process com)
      )
    (cond ((equal type 'gp-eval-insert)
	   (while (not last-gp-result)
	     (message "waiting for result...")
	     (sleep-for 1)
	     )
	   (message "done")
	   (setq tem (gp-trim-result  last-gp-result))
	   (setq last-gp-result nil)
	   tem))))

(defun gp-trim-result (x)
  (let (tem)
    (cond ((equal 0 (count-expr "\n" x))
	   (setq x "OK")
	   x)
	  ((equal 1 (count-expr "\n" x))
	   (cond ((string-match "%[0-9]+[ ]=" x)
		  (setq x (substring x (match-end 0)))))
	   (cond ((setq tem (string-match "\n$" x))
		  (setq x (substring x 0 tem))))
	   x)
	  (t (cond ((setq tem (string-match "%[0-9]+[ ]=" x))
		    (while (< tem (match-end 0))
		      (aset x tem ? )
		      (setq tem (+ tem 1)))))
             x)) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; code for Splus evaluation.;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; make Splus-eval-insert face do insertion of result
;;
(def-book-face  'Splus-eval-insert 'Splus-eval 'underline "LimeGreen" "black" )

(put 'Splus-eval-insert 'insert t)

(def-book-face 'Splus-eval 'Splus-eval 'underline "LimeGreen" "black" )

(defvar Splus-eval nil)

(defun book-mark-for-Splus-eval (eval-only beg end)
  "Mark the current region to be evaluated by Splus and
substituted in the next book result region.   If a
numeric arg is supplied, dont wait for the evaluation nor
insert the result.  The variable book-maxima-auto-result-insert
affects whether a sample result is inserted.
"
  (interactive "P\nr")
  (put-text-property beg
		     end 
		     'face (if eval-only 'Splus-eval 'Splus-eval-insert))
  (put-text-property beg
		     end
		     'mouse-face 'book-mouse-face)  
  (or eval-only
      (not book-maxima-auto-result-insert)
      (book-result-next end)
      (save-excursion (goto-char end)
		      (insert
		       (nth (random (length book-maxima-auto-result-insert))
			    book-maxima-auto-result-insert))
		      (book-insert-sample-result)))
  (show-saved-properties beg)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar book-Splus-ready-for-input nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar last-Splus-result nil)
(defun book-Splus-process-filter ( proc str)
  (let (tem )
    (book-process-filter proc str)
    (put-process-prop proc 'last-output
		      (concat (get-process-prop proc 'last-output)
			      str))
    (cond ((setq tem (string-match ">[ ]*$"
				   (get-process-prop proc 'last-output)))
	   (setq book-Splus-ready-for-input t)
	   (setq last-Splus-result
		 (substring (get-process-prop proc 'last-output)  0 tem ))
	   (put-process-prop proc 'last-output "")
	   )
	  )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Splus-restart ()
  (setq Splus-eval nil)
  (if (get-buffer "*Splus-eval*")
      (kill-buffer (get-buffer "*Splus-eval*") )))

(defun book-Splus-interrupt ()
  "Interrupt the *Splus-eval* process running for book mode"
  (interactive)
  (cond (Splus-eval
	 (interrupt-process Splus-eval))
	(t (error "*Splus-eval* process not found"))))

(defun Splus-eval (beg end type)
  "Evaluate the region returning a result"
  (let (tem (process (get-process  "*Splus-eval*")))
    (cond ((not  (and Splus-eval
		      (setq process (get-buffer-process Splus-eval))))
	   (cond ((and under-x-windows x-display-name
		       (not (getenv "DISPLAY")))
		  (setq process-environment
			(cons (concat "DISPLAY=" x-display-name)
			      process-environment))))
	   (let ((default-directory "~/"))
	     (setq Splus-eval (make-sshell "Splus-eval" "Splus" )))
	   (setq process (get-buffer-process  Splus-eval))
	   (set-process-filter process 'book-Splus-process-filter)
	   (process-send-string process  "\n")
	   )
	  )
    (let ((com (buffer-substring beg end)))
      (setq com (concat com "\n"))
      (while (not book-Splus-ready-for-input)
	(message "waiting till Splus ready for input..")
	(process-send-string process "\n")
	(sleep-for 1))
      (message  "sending command :%s " com )
      
      (setq last-Splus-result nil)
      (process-send-string process com)
      )
    (cond ((equal type 'Splus-eval-insert)
	   (while (not last-Splus-result)
	     (message "waiting for result...")
	     (sleep-for 1)
	     )
	   (message "done")
	   (setq tem (Splus-trim-result  last-Splus-result))
	   (setq last-Splus-result nil)
	   tem))))

(defun Splus-trim-result (x)
  (let (tem)
    (cond ((equal 0 (count-expr "\n" x))
	   (setq x "OK")
	   x)
	   ((equal 1 (count-expr "\n" x))
	   (cond ((string-match "\[[0-9]+\]" x)
		  (setq x (substring x (match-end 0)))))
	   (cond ((setq tem (string-match "\n$" x))
		  (setq x (substring x 0 tem))))
	   (cond ((not (string-match "\n" x))
		  (cond ((string-match "[ \t]+" x)
			 (setq x (substring x (match-end 0)))))))
	   x)
	  (t x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; code for Mathematica evaluation.;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; make mma-eval-insert face do insertion of result
;;
(def-book-face  'mma-eval-insert 'mma-eval 'underline "yellow3" "black" )
(put 'mma-eval-insert 'insert t)
(def-book-face 'mma-eval 'mma-eval 'underline "yellow3" "black" )
(defvar mma-eval nil)

(defun book-mark-for-mma-eval (eval-only beg end)
  "Mark the current region to be evaluated by Mathematica and
substituted in the next book result region.   If a
numeric arg is supplied, dont wait for the evaluation nor
insert the result.  The variable book-maxima-auto-result-insert
affects whether a sample result is inserted.
"
  (interactive "P\nr")
  (put-text-property beg
		     end 
		     'face (if eval-only 'mma-eval 'mma-eval-insert))
  (put-text-property beg
		     end
		     'mouse-face 'book-mouse-face)  
  (or eval-only
      (not book-maxima-auto-result-insert)
      (book-result-next end)
      (save-excursion (goto-char end)
		      (insert
		       (nth (random (length book-maxima-auto-result-insert))
			    book-maxima-auto-result-insert))
		      (book-insert-sample-result)))
  (show-saved-properties beg)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar book-mma-ready-for-input nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar last-mma-result nil)
(defun book-mma-process-filter ( proc str)
  (let (tem )
    (book-process-filter proc str)
    (put-process-prop proc 'last-output
		      (concat (get-process-prop proc 'last-output)
			      str))
    (cond ((setq tem (string-match "In\[[0-9]+\]:=[ ]*$"
				   (get-process-prop proc 'last-output)))
	   (setq book-mma-ready-for-input t)
	   (setq last-mma-result
		 (substring (get-process-prop proc 'last-output)  0 tem ))
	   (put-process-prop proc 'last-output "")
	   )
	  )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mma-restart ()
  (setq mma-eval nil)
  (if (get-buffer "*mma-eval*")
      (kill-buffer (get-buffer "*mma-eval*") )))

(defun book-mma-interrupt ()
  "Interrupt the *mma-eval* process running for book mode"
  (interactive)
  (cond (mma-eval
	 (interrupt-process mma-eval))
	(t (error "*mma-eval* process not found"))))

(defun mma-eval (beg end type)
  "Evaluate the region returning a result"
  (let (tem (process (get-process  "*mma-eval*")))
    (cond ((not  (and mma-eval
		      (setq process (get-buffer-process mma-eval))))
	   (cond ((and under-x-windows x-display-name
		       (not (getenv "DISPLAY")))
		  (setq process-environment
			(cons (concat "DISPLAY=" x-display-name)
			      process-environment))))
	   (let ((default-directory "~/"))
	     (setq mma-eval (make-sshell "mma-eval" "math" )))
	   (setq process (get-buffer-process  mma-eval))
	   (set-process-filter process 'book-mma-process-filter)
	   (process-send-string process  "\n")
	   )
	  )
    (let ((com (buffer-substring beg end)))
      (setq com (concat com "\n"))
      (while (not book-mma-ready-for-input)
	(message "waiting till Mathematica ready for input..")
	(process-send-string process "\n")
	(sleep-for 1))
      (message  "sending command :%s " com )
      
      (setq last-mma-result nil)
      (process-send-string process com)
      )
    (cond ((equal type 'mma-eval-insert)
	   (while (not last-mma-result)
	     (message "waiting for result...")
	     (sleep-for 1)
	     )
	   (message "done")
	   (setq tem (mma-trim-result  last-mma-result))
	   (setq last-mma-result nil)
	   tem))))
;;;;
(defun mma-trim-result (str)
  (let ( (tem) (x) )
    (setq x str) 
    ;;
    ;; if there is output at all, x is at least of length 2
    ;; including a trailling \n\n.
    ;; Strip the beginning \n and one ending \n out.
    ;;
    (if (< (length x) 2) 
	(setq x "OK")
      (cond ((string-match "Out\[[0-9]+\]=" x)
	     (setq x (substring x 1 (- (length x ) 1))))))
    ;;
    (cond ((equal 1 (count-expr "\n" x))
	   (cond ((string-match "Out\[[0-9]+\]=[ ]+" x)
		  (setq x (substring x (match-end 0)))))
	   (cond ((setq tem (string-match "\n$" x))
		  (setq x (substring x 0 tem))))
	   x)
	  (t (cond ((setq tem (string-match "Out\[[0-9]+\]=[ ]" x))
		    (while (< tem (match-end 0))
		      (aset x tem ? )
		      (setq tem (+ tem 1)))))
	     x))))
;;;
(provide 'bookmode)

;;; Local Variables: ***
;;; version-control: t ***
;;; End: ***

