;; Copyright (C) 2007, 2008 Yasuaki Honda

;; Author: Yasuaki Honda (yhonda@mac.com)
;; $Id: setup-imaxima-imath.el,v 1.6 2009-02-22 09:18:27 yasu-honda Exp $

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

;;;
;;; Set up Emacs for Imaxima with NTEmacs, Meadow, MiKTeX, and Ghostscript
;;;

(defvar *imaxima-miktex-bin-dir*
  (file-name-directory (car (file-expand-wildcards "c:/Program Files*/MiKTeX*/miktex/bin/latex.exe")))
  "MiKTeX bin directory, added to exec-path")

(defvar *imaxima-maxima-el-dir*
  (file-name-directory (car (file-expand-wildcards "c:/Program Files*/Maxima*/share/maxima/*/emacs/maxima.el")))
  "Maxima emacs mode maxima.el directory, added to load-path")

(defvar *imaxima-maxima-bin-dir*
  (file-name-directory (car (file-expand-wildcards "c:/Program Files*/Maxima*/bin/maxima.bat")))
  "Maxima bin directory, added to exec-path")

(defvar *imaxima-maxima-info-dir*
  (file-name-directory (car (file-expand-wildcards "c:/Program Files*/Maxima*/info/maxima.info")))
  "Maxima info directory, added to Info-additional-directory-list")

(if (not (boundp 'Info-additional-directory-list))
    (setq Info-additional-directory-list nil))

;;; set up maxima command to maxima.bat, which resides in the above path.
;;(setq maxima-command "maxima.bat")

;;; maxima.bat is specified as the maxima program for imaxima.
(setq imaxima-maxima-program "maxima.bat")

;;; latex.exe is specified for the latex program to be invoked.
(setq imaxima-tex-program "latex.exe")

(defvar *imaxima-imath-dir* *imaxima-maxima-el-dir*
  "Imaxima imath directory, containing .el, .lisp, and .info files")

;;; The following definition eases the locating of imaxima.lisp.
(setq imaxima-lisp-file (concat *imaxima-imath-dir* "imaxima.lisp"))

;;; Ghostscript bin directory is added to the exec-path.

(defvar *imaxima-gs-bin-dir*
  (file-name-directory (car (file-expand-wildcards "c:/Program Files*/gs/gs*/bin/gswin*.exe")))
  "Ghostscript bin directory")

(if (file-expand-wildcards "c:/Program Files*/gs/gs*/bin/gswin32c.exe")
    (setq imaxima-gs-program "gswin32c.exe")
  (if (file-expand-wildcards "c:/Program Files*/gs/gs*/bin/gswin64c.exe")
      (setq imaxima-gs-program "gswin64c.exe")))

;;; set up exec-path
(setq exec-path (append (list *imaxima-gs-bin-dir* *imaxima-maxima-bin-dir* *imaxima-miktex-bin-dir*) exec-path))

;;; set up load-path
(setq load-path (append (list *imaxima-imath-dir* *imaxima-maxima-el-dir*) load-path))

;;; set up Info-additional-directory-list
(setq Info-additional-directory-list (append (list *imaxima-imath-dir* *imaxima-maxima-info-dir*) Info-additional-directory-list))


;;; imaxima-maxima-options should be "" instead of "(user::run)"
;;; assigned in the imaxima.el
(setq imaxima-maxima-options "")

(autoload 'maxima "maxima" "Maxima CAS mode" t nil)

(autoload 'imaxima "imaxima" "Graphical frontend for Maxima CAS" t nil)

(autoload 'imath-mode "imath" "Math text mode" t nil)
