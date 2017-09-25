


;*******************************************************************************
;*                                                                             *
;*  copyright (c) 1988 kent state univ.  kent, ohio 44242                      *
;*                                                                             *
;*******************************************************************************


;;  ---------  ;;                 load
;;  gtload.l   ;;    gentran code generation package
;;  ---------  ;;              for vaxima

;;(cond ((not (boundp '*gentran-dir))
 ;;      (setq *gentran-dir (getenv "GENTRAN"))))

(in-package :maxima)

(defvar local-obj-dir)

(load (merge-pathnames "convmac.lisp"
		       #-gcl *load-pathname*
		       #+gcl sys:*load-pathname*))

(putprop 'procforttem "templt" 'autoload)
(putprop 'procrattem "templt" 'autoload)
(putprop 'procctem "templt" 'autoload)
(putprop '$readvexp "templt" 'autoload)
(putprop 'gentranparse "parser" 'autoload)
(putprop 'opt "opt"    'autoload)
;(putprop 'seg "segmnt" 'autoload)
(putprop 'fortcode "lspfor" 'autoload)
(putprop 'ratcode "lsprat" 'autoload)
(putprop 'ccode "lspc"   'autoload)

(dolist (fname  '( "init" "lspfor" "templt" "global" "intrfc"
		     "pre" "output" "vaxlsp" "segmnt"))
               (load (merge-pathnames fname
				      #-gcl *load-pathname*
				      #+gcl sys:*load-pathname*)))
