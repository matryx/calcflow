;;;; imath.el --- Imath minor mode

;; Copyright (C) 2004, 2008 Yasuaki Honda
;; Copyright (C) 2007 bohumir for the {eps eps} formula support

;; Author: Yasuaki Honda (yhonda@mac.com)
;; Created: 7 Nov 2004
;; Version: 1.0b
;; Keywords: maxima

;; $Id: imath.el,v 1.5 2009-02-22 09:18:27 yasu-honda Exp $

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

;; How to install imath minor mode
;;  1. You need to install imaxima properly. (./configure;make;make install)
;;  2. Put (autoload 'imath-mode "imath" "Interactive Math minor mode." t)
;;  in the .emacs.el file.
;;
;; How to use imath minor node
;;  1. In a buffer, M-x imath-mode to enable imath minor mode.
;;  2. C-c [ to insert a template maxima form.
;;     C-c ] to insert a template latex form.
;;  3. Type any Maxima command input in the maxima template form:
;;     Ex: {maxima diff(f(x),x) maxima}
;;     Type any LaTeX command in the latex template form:
;;     Ex: {latex  \ifracd{d}{d\*x}\*f\left(x\right) latex}
;;  4. Type C-c ! to obtain an image of the formula.
;;  5. If you want to edit the maxima or latex command, place cursor
;;     right after the image and type C-c & to obtain the original
;;     maxima or latex form.
;;  6. You can save the buffer, however discarding all then formula
;;     images.  Formulas are saved as maxima forms or latex forms or
;;     both. On visiting the saved file, you can restore all the
;;     images by enabling the imath minor mode and then type C-c $.
;;  7. Imath mode can be automatically set by having the first line
;;     of the file like this:
;;     ;; -*- mode: imath -*-
;;  8. Imath mode enables you to export the buffer contents to
;;     HTML document. You can do so by typing M-x imath-to-html .
;;     A buffer is created to convert imath text to HTML. C-x C-s
;;     saves the buffer to the file whose name is the same as the
;;     original file but file extension being .html in the same
;;     folder as the original file.
;;     A folder is created to store all the formula images. They
;;     are referenced from the HTML document by using <IMG> tag.

(require 'cl)
(require 'imaxima)

(if (featurep 'xemacs)
    (require 'atomic-extents))

(define-minor-mode imath-mode
  "Toggle MathEdit mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.

The imath minor mode provides a small set of functions to aid
insert math formulas into plain text. 

A math formula is written using a Maxima form whose syntax is
{maxima a formula maxima} where a formula is a string which can
be accepted as Maxima command input. C-c [ inserts a template
for a maxima form. 

The other way to write a math formula is to use LaTeX form
whose syntax is {latex a formula latex} where a formula is 
a valid LaTeX commands. C-c ] inserts a template for a latex
form.

Example maxima and latex forms are:
{maxima integrate(f(x),x) maxima}
{maxima sum(a[n],n,0,i) maxima}
{latex  \\int {f\\left(x\\right)}{\\;dx} latex}
{latex  \\sum_{n=0}^{i}{a_{n}} latex}

Version 1.0 beta and later supports inline graph. You can use
the following six maxima commands in maxima forms:
wxplot2d(), wxplot3d(), wxdraw2d(), wxdraw3d(), wximplicit_plot(),
wxcontour_plot().

Example maxima forms for inline graph are:
{maxima wxplot2d(sin(5*x)/x,[x,-5,5]) maxima}
{maxima wxplot3d(sin(x)*y,[x,-5,5],[y,-5,5]) maxima}
{maxima wxdraw2d(implicit(x^2+y^3=5,x,-5,5,y,-5,5)) maxima}

Removing the prefix \"wx\" from the commands above, you obtain
command names that are described in Maxima manual. The documentation
applies to the \"wx\" version of the commands.

Version 1.0 beta and later supports inline EPS image. You can
insert arbitrary image files in EPS format using the following
syntax:
{eps filename eps}

An example is:
{eps /home/george/image1.eps eps}

Assuming the cursor position is right after a form or in the
middle, C-c ! transforms the form into the formula image using
the Imaxima functionality.

If the resulting image is not what you want, you may want to edit
the formula again. To do this, place the cursor right after the
image and C-c &. Then the image is removed and original form
appears at the position.

When saving the buffer into a file, images are
discarded. However, maxima forms and their correspoding latex
forms are kept there in the text. If the text is loaded again
into Emacs and imath minor mode is enabled, you can type C-c $ to
restore all the images for the forms in the buffer.

Imath mode enables you to export the buffer contents to
HTML document. You can do so by typing M-x imath-to-html .
A buffer is created to convert imath text to HTML. C-x C-s
saves the buffer to the file whose name is the same as the
original file but file extension being .html in the same
folder as the original file.

A folder is created to store all the formula images. They
are referenced from the HTML document by using <IMG> tag.
"
     
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " iMath"
  ;; The minor mode bindings.
  '(("\C-c[" . compose-maxima-formula)
    ("\C-c]" . compose-latex-formula)
    ("\C-c!" . form-to-image)
    ("\C-c$" . buffer-formula-to-image)
    ("\C-c&" . remove-maxima-formula-image)
    )
  ;; body
  ;; Imath requires *imaxima* running.
  (progn
    (let ((cur-buf (current-buffer)))
      (save-current-buffer
	(imaxima))
      (switch-to-buffer cur-buf))))

;;; Continuation is used between maxima-to-image function and
;;; get-image-from-imaxima. The value is either nil or a list of
;;; buffer, pos1, and pos2, where pos1 and pos2 are the beginning and
;;; end of current maxima formula.
;;  (func buffer pos1 pos2)

(defvar continuation nil)
(defvar maxima-start "{maxima ")
(defvar maxima-end " maxima}")
(defvar latex-start "{latex ")
(defvar latex-end " latex}")
(defvar eps-start "{eps ")
(defvar eps-end " eps}")

(defun compose-maxima-formula ()
  "Insert maxima form template at the current position."
  (interactive "")
  (insert "{maxima  maxima}")
  (backward-char 8))

(defun compose-latex-formula ()
  "Insert maxima form template at the current position."
  (interactive "")
  (insert "{latex  latex}")
  (backward-char 7))

(defun* find-formula (ftype)
  (let (start-symbol end-symbol tmpresult)
    (cond ((eql ftype 'maxima)
	   (setq start-symbol maxima-start
		 end-symbol maxima-end))
	  ((eql ftype 'latex)
	   (setq start-symbol latex-start
		 end-symbol latex-end))
          ((eql ftype 'eps)
	   (setq start-symbol eps-start
		 end-symbol eps-end))
	  ((eql ftype 'both)
	   (save-excursion
	     (multiple-value-bind (la-start la-end la-type)
		 (find-formula 'latex)
	       (if (not (and la-start la-end la-type))
		   (return-from find-formula nil)
		 (goto-char (1- la-start))
		 (if (not (string= (buffer-substring (point) (1+ (point)))
				   "&"))
		     (return-from find-formula nil))
		 (multiple-value-bind (mx-start mx-end mx-type)
		     (find-formula 'maxima)
		   (if (not (and mx-start mx-end mx-type))
		       (return-from find-formula nil))
		   (return-from find-formula
		     (values mx-start la-end 'both)))))))
	  ((eql ftype 'any)
	   (cond ((setq tmpresult (find-formula 'both))
		  (return-from find-formula tmpresult))
		 ((setq tmpresult (find-formula 'latex))
		  (return-from find-formula tmpresult))
		 ((setq tmpresult (find-formula 'maxima))
		  (return-from find-formula tmpresult))
		 ((setq tmpresult (find-formula 'eps))
		  (return-from find-formula tmpresult))
		 (t 
		  (return-from find-formula nil))))
	  (t (return-from find-formula nil)))
    (save-excursion
      (let (begin end (curpos (point)))
	(setq begin (search-backward start-symbol (point-min) t))
	(setq end (search-forward end-symbol (point-max) t))
	(if (and (numberp begin) (numberp end) ;; {start-symbol ... end-symbol} is found.
		 (or (and (> end curpos) (> curpos begin)) ;; {start-symbol ... curpos ... end-symbol}
		     (= curpos end))) ;;  {start-symbol ... end-symbol}}curpos
	    (values begin end ftype)
	  nil)))))

(defun remove-maxima-formula-image (arg)
  (interactive "P")
  (save-excursion
    (multiple-value-bind (begin end ftype)
	(find-formula 'any)
      (when (and begin end ftype)
	(if (featurep 'xemacs)
	    (let ((ext (extent-at begin)))
	      (if ext (delete-extent ext)))
	  (remove-text-properties begin end '(display) (current-buffer)))
	(if (eql ftype 'both)
	    (multiple-value-bind (la-begin la-end la-ftype)
		(find-formula 'latex)
	      (when (and (not arg) la-begin la-end la-ftype)
		;; remove & between the maxima formula and latex formula
		;; if that is the case.
		(if (eql ftype 'both) (decf la-begin))
		(delete-region la-begin la-end))))))))

(defun form-to-image ()
  "Convert any form to image based on form types"
  (interactive "")
  (multiple-value-bind (start end ftype)
      (find-formula 'any)
    (if (and start end ftype)
	(cond ((eql ftype 'maxima)
	       (maxima-to-image))
	      ((eql ftype 'eps)
	       (get-image-from-imaxima 'eps))
	      ((or (eql ftype 'both)
		   (eql ftype 'latex))
	       (get-image-from-imaxima 'latex)
	       )
	      )
      )
    )
  )

(defun* maxima-to-image (&aux maxcmd)
  "Transform a maxima form which is placed just before current point or
is surrounding the current point into a formula image."
  (interactive "")
  (save-excursion
    (multiple-value-bind (begin end)
	(find-formula 'maxima)
      (let (curpos (point))
	(when (and begin end)
	  (setq maxcmd (buffer-substring (+ begin (length maxima-start))
				      (- end (length maxima-end))))
	  (kill-new (buffer-substring (+ begin (length maxima-start))
				      (- end (length maxima-end))))
	  (save-excursion
	    (set-buffer (if imaxima-use-maxima-mode-flag
			    "*maxima*"
			  "*imaxima*"))
	    (yank)
	    (insert ";")
	    (comint-send-input))))
      (if continuation
	  (debug-imaxima-filter "continuation exits"))
      (setq continuation (list (if (string-match "[ 	]*wx\\(plot2d\\|plot3d\\|draw\\|draw2d\\|draw3d\\|implicit_plot\\|contour_plot\\).*" maxcmd)
				   #'get-inline-graph
				 #'get-image-from-imaxima-1)
			       (current-buffer) begin end)))))

(defun* maxima-to-image-all (&aux maxcmd)
  "Transform a maxima form which is placed just before current point or
is surrounding the current point into a formula image."
  (interactive "")
  (if (not (re-search-forward "maxima}[^&]" (point-max) 0))
      (progn
	(setq continuation nil)
	(return-from maxima-to-image-all nil)))
  (backward-char)
  (multiple-value-bind (begin end)
      (find-formula 'maxima)
    (when (and begin end)
      (setq maxcmd (buffer-substring (+ begin (length maxima-start))
				     (- end (length maxima-end))))
      (kill-new (buffer-substring (+ begin (length maxima-start))
				  (- end (length maxima-end))))
      (save-excursion
	(set-buffer (if imaxima-use-maxima-mode-flag
			"*maxima*"
		      "*imaxima*"))
	(yank)
	(insert ";")
	(comint-send-input))
      (if continuation
	  (debug-imaxima-filter "continuation exits"))
      (setq continuation (list (if (string-match "[ 	]*wx\\(plot2d\\|plot3d\\|draw\\|draw2d\\|draw3d\\|implicit_plot\\|contour_plot\\).*" maxcmd)
				   #'(lambda (arg)
				   (let ((cont continuation))
				     (funcall #'get-inline-graph arg)
				     (save-current-buffer
				       (set-buffer (nth 1 cont))
				       (maxima-to-image-all))))
			     #'(lambda (arg)
				 (let ((cont continuation))
				   (funcall #'get-image-from-imaxima-1 arg)
				   (save-current-buffer
				     (set-buffer (nth 1 cont))
				     (maxima-to-image-all)))))
			   (current-buffer) begin end)))))


(defun* get-image-from-imaxima (eps-or-latex)
  "Converts a both form or a latex form into a formula image when
placed right after the form."
  (interactive "")
  (let (la-start la-end la-ftype entire-start entire-end entire-ftype
		 entire-string latex-formula-or-epsfile)

    (multiple-value-bind (la-start la-end la-ftype)
	(find-formula eps-or-latex)
      (when (and la-start la-end la-ftype)
	(setq entire-string (buffer-substring la-start la-end))
	(cond ((eql eps-or-latex 'latex)
	       (setq latex-formula-or-epsfile (substring entire-string
							 (length latex-start)
							 (- (length entire-string)
							    (length latex-end))))
	       )
	      ((eql eps-or-latex 'eps)
	       (setq latex-formula-or-epsfile (substring entire-string
							 (length eps-start)
							 (- (length entire-string)
							    (length eps-end))))
	       )
	      )
	(multiple-value-bind (entire-start entire-end entire-ftype)
	    (find-formula 'any)
	  (when (and entire-start entire-end entire-ftype)
	    (if (featurep 'xemacs)
		(progn
		  (let ((ext (extent-at entire-start)))
		    (if ext
			(return-from get-image-from-imaxima nil)))
		  
		  (cond ((eql eps-or-latex 'latex)
			 (let ((ext (extent-at 0 (imaxima-make-image latex-formula-or-epsfile eps-or-latex  t))))
			   (if ext
			       (insert-extent ext entire-start entire-end t (current-buffer))))
			 )
			((eql eps-or-latex 'eps)
			 (let ((ext (extent-at 0 (imaxima-make-image latex-formula-or-epsfile eps-or-latex  t))))
			   (if ext
			       (insert-extent ext entire-start entire-end t (current-buffer))))
			 )
			)
		  )

	      ;; FSF Emacs
	      (add-text-properties entire-start entire-end
				   (list 'display
					 (get-text-property 1
							    'display
							    (imaxima-make-image latex-formula-or-epsfile eps-or-latex t)))
				   (current-buffer)))))))))

(defun get-image-from-imaxima-1 (latex-string)
  (setq latex-string (copy-sequence latex-string))
  (if (featurep 'xemacs)
      (let ((ext (extent-at 0 latex-string)))
	(if ext (set-extent-property ext 'duplicable nil))))
  (while (string-match "^([\\%a-zA-Z0-9]+)" latex-string)
    (setq latex-string (replace-match "" t t latex-string)))
  (if continuation
      (let ((maxima-string (save-current-buffer
			     (set-buffer (nth 1 continuation))
			     (buffer-substring (nth 2 continuation)
					       (nth 3 continuation))))
	    pos)
	(save-current-buffer
	  (set-buffer (nth 1 continuation))
	  (setq pos (copy-marker (nth 2 continuation) t))
	  (delete-region (nth 2 continuation) (nth 3 continuation))
	  (setq continuation nil)
	  (let ((str-to-insert (concat maxima-string "&{latex " latex-string " latex}")))
	    (if (featurep 'xemacs)
		(let ((ext (extent-at 0 (imaxima-make-image latex-string 'latex t))))
		  (if ext
		      (insert-extent ext 0 (length str-to-insert) t str-to-insert)))
	      (add-text-properties 0 (length str-to-insert)
				   (list 'display
					 (get-text-property 1
							    'display
							    (imaxima-make-image latex-string 'latex  t)))
				   str-to-insert))
	    (goto-char pos)
	    (insert str-to-insert))))))

(defun get-inline-graph (latex-string)
  (setq latex-string (copy-sequence latex-string))
  (if (featurep 'xemacs)
      (let ((ext (extent-at 0 latex-string)))
	(if ext (set-extent-property ext 'duplicable nil))))
  (when (string-match "^([\\%a-zA-Z0-9]+)" latex-string)
    (setq latex-string (replace-match "" t t latex-string)))
  (if continuation
      (let ((maxima-string (save-current-buffer
			     (set-buffer (nth 1 continuation))
			     (buffer-substring (nth 2 continuation)
					       (nth 3 continuation)))))
	(save-current-buffer
	  (set-buffer (nth 1 continuation))
	  (delete-region (nth 2 continuation) (nth 3 continuation))
	  (setq continuation nil)
	  (let ((str-to-insert maxima-string))
	    (if (featurep 'xemacs)
		(let ((ext (extent-at 0 (imaxima-make-image latex-string 'latex t))))
		  (if ext
		      (insert-extent ext 0 (length str-to-insert) t str-to-insert)))
	      (add-text-properties 0 (length str-to-insert)
				   (list 'display
					 (get-text-property 1 'display latex-string))
				   str-to-insert))
	    (insert str-to-insert))))))


(defun buffer-formula-to-image ()
  "Transform all the latex forms and maxima&latex forms into
formula images." 
  (interactive "")
  (beginning-of-buffer)
  (let ((msg "Converting latex"))
    (message msg)
    (while (search-forward latex-end nil 1)
      (get-image-from-imaxima 'latex)
      (setq msg (concat msg "."))
      (message msg))
    (message (concat msg ".done.")))
  (beginning-of-buffer)
  (let ((msg "Converting eps"))
    (while (search-forward eps-end nil 1)
      (get-image-from-imaxima 'eps)
      (setq msg (concat msg "."))
      (message msg))
    (message (concat msg ".done.")))
  (beginning-of-buffer)
  (let ((msg "Converting maxima..."))
    (message msg)
    (maxima-to-image-all)
    (message (concat msg "done.")))
  (set-buffer-modified-p nil))


(provide 'imath)

;;; imath.el ends here
