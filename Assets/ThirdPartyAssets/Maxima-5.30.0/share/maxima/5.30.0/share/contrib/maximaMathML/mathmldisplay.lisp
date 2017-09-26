;;;;;;;;;;;;;;;;; File:  mathmldisplay.lsp  ;;;;;;;;;;;;;;;;;;;;;
;; Purpose:   Makes maxima display mathml
;;
;; Usage:     load this file into maxima 
;;                loadfile("mathmldisplay.lsp");
;;
;; Author: Paul S. Wang
;; Date: 5/20/99
;
; Authors:  Paul S. Wang, Kent State University
; This work was supported by NSF/USA.
; Permission to use this work for any purpose is granted provided that
; the copyright notice, author and support credits above are retained.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(defun displa(exp) 
     (setq exp (caddr exp))   ;;; get rid of output label
     (print 'presentation-exp)
     ($prmathml exp)
     (print 'content-exp)
     ($ctmathml exp)
     (print 'end-exp)
     (terpri)
)

;;;;;; example usage

;;   (C1) loadfile("mathmldispla.lsp");      
                                     
;;   mathmldispla.lsp being loaded.          
;;   Loading mathmldispla.lsp                
;;   Finished loading mathmldispla.lsp       
