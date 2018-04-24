;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;                 GJC 10:11pm  Tuesday, 14 July 1981                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module trprop)

;; Many macsyma extension commands, e.g. $INFIX, $TELLSIMP,
;; $DEFTAYLOR work by doing explicit PUTPROPS.
;; These META-PROP functions allow selected commands to
;; also output DEFPROP's when processed in the Macsyma->lisp translation.

(defmvar meta-prop-p nil)
(defmvar meta-prop-l nil)

(defun meta-output (form)
  (if *in-translate-file* (push form meta-prop-l))
  ;; unfortunately, MATCOM needs to see properties in order
  ;; to compose tellsimps. so eval it always.
  (eval form))

(defmfun meta-add2lnc (item ssymbol)
  (if meta-prop-p
      (meta-output `(add2lnc ',item ,ssymbol))
      (add2lnc item (symbol-value ssymbol))))

(defmfun meta-putprop (ssymbol item key)
  (if meta-prop-p
      (prog1 item (meta-output `(defprop ,ssymbol ,item ,key)))
      (putprop ssymbol item key)))

(defmfun meta-mputprop (ssymbol item key)
  (if meta-prop-p
      (prog1 item (meta-output `(mdefprop ,ssymbol ,item ,key)))
      (mputprop ssymbol item key)))

(defmfun meta-fset (ssymbol definition)
  (if meta-prop-p
      (prog1 definition (meta-output
			 `(fset ',ssymbol (coerce ',definition 'function))))
      (fset ssymbol (coerce definition 'function))))

