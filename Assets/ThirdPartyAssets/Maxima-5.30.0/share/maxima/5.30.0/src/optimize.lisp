;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :maxima)

(import '(compiler::inline-unsafe compiler::inline-always compiler::boolean
	  compiler::definline ) 'cl-maxima)

(macsyma-module rat3f)


(defmacro definline
    (property return-type side-effect-p new-object-p name arg-types
     body)
  `(progn
    (si::remprop ',name 'compiler::fixed-args)
    (push 
     '(,arg-types ,return-type ,(logior (if new-object-p 1 0) (if side-effect-p 2 0))
       ,body)
     (get ',name ',property))))

(defun proclaim-property (arg)
  (let ((prop (car arg)))
    (loop for v in (cdr arg)
	   do (assert (symbolp v))
	   (setf (get v prop) t))))



(progn
  (definline inline-always t nil nil ctimes (t t) "ctimes(#0,#1)")
  (definline inline-always t nil nil cplus (t t) "cplus(#0,#1)")
  (definline inline-always t nil nil cdifference (t t) "cdifference(#0,#1)")
  (definline inline-always t nil nil cmod (t ) "cmod(#0)")
  )

(definline inline-unsafe fixnum nil nil quot (fixnum fixnum)
	   "((#0)/(#1))")

(definline inline-unsafe fixnum nil nil rem (fixnum fixnum)
	   "((#0)%(#1))")

(definline inline-always boolean nil nil bigp (t)
	   "(type_of(#0)==t_bignum)")


(definline inline-always boolean nil nil pointergp (t t)
           "((fix((#0)->s.s_dbind)) > fix(((#1)->s.s_dbind)))")


;;(and (not (consp x)) (if (si::fixnump x) (= 0 (the fixnum x)) (zerop x)))
(definline inline-always boolean nil nil pzerop (t )
	   "@0;(type_of(#0)==t_fixnum ?  (fix(#0)==0)
       :type_of(#0) == t_cons ? 0 
       :type_of(#0)==t_shortfloat ? (sf(#0)==0.0)
       :(type_of(#0)==t_longfloat && (lf(#0)==0.0)))")

;;(definline inline-always boolean nil nil pzerop (t )
;;  "(type_of(#0)==t_fixnum ?  (fix(#0)==0)
;;       :type_of(#0)==t_shortfloat ? (sf(#0)==0.0)
;;       :(type_of(#0)==t_longfloat && (lf(#0)==0.0)))")








