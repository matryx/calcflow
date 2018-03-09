;;;; colorterm.lisp -- A rudimentary implementation of colored input/output
;;;;                   in terminal mode.

;;;; Copyright (C) 2008 James F. Amundson

;;;; colorterm.lisp is free software; you can redistribute it
;;;; and/or modify it under the terms of the GNU General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.

;;;; colorterm.lisp is distributed in the hope that it will be
;;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;; See the GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with colorterm.lisp; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, Inc., 59 Temple Place -
;;;; Suite 330, Boston, MA 02111-1307, USA.

;;;; Notes:
;;;; To use this file in interactive mode, do
;;;;   load("colorterm.lisp");colorterm();
;;;; To use this file every time you launch maxima, add the above line
;;;; to maxima-init.mac in $HOME/.maxima

;;;; Known bug: colorterm interacts poorly with rlwrap. When rlwrap is
;;;; being used to run maxima (e.g., with rmaxima) extra characters appear
;;;; after the prompt. These characters are confusing, but do not appear as
;;;; part of the actual input.

;;; Default colors. For color mappings, see comments below.
(defvar *prompt-color* 31)
(defvar *input-color* 34)
(defvar *output-color* 30)
;;;  Foreground Colours
;;;  30	Black
;;;  31	Red
;;;  32	Green
;;;  33	Yellow
;;;  34	Blue
;;;  35	Magenta
;;;  36	Cyan
;;;  37	White

(defun $colorterm ()
    (let ((escape-char (code-char 27)))
        (setf *prompt-prefix* 
            (format nil "~a[00;~am" escape-char *prompt-color*))
        (setf *prompt-suffix* 
            (format nil "~a[00;~am" escape-char *input-color*))
        (setf *general-display-prefix* 
            (format nil "~a[00;~am" escape-char *output-color*))
        (setf *maxima-epilog* 
            (format nil "~a[00m" escape-char))))


    

