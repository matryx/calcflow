(in-package :common-lisp-user)

(defpackage :maxima-nregex
  (:use :common-lisp)
  (:export
   ;; vars
   #:*regex-debug* #:*regex-groups* #:*regex-groupings*
   ;; functions
   #:regex-compile
   ))

(defpackage :cl-info
  (:use :common-lisp))

(defpackage :command-line
  (:use :common-lisp)
  (:nicknames :cmdline)
  (:export #:cl-option #:make-cl-option #:list-cl-options #:process-args
	   #:get-application-args))

;; Kevin Rosenberg's getopt package
(defpackage getopt
  (:use :cl)
  (:export #:match-unique-abbreviation
	   #:getopt))

;; GCL has SLOOP built in but it's slightly different now...
(defpackage :cl-sloop
  (:use :common-lisp)
  (:export #:sloop))

(defpackage :maxima
  (:use :common-lisp :command-line)
  ;; Gcl has DEFINE-COMPILER-MACRO but it's in the SYSTEM package.  So
  ;; we shadowing import it into our package here.  (Can't just import
  ;; because there's already a DEFINE-COMPILER-MACRO symbol.)
  #+gcl
  (:shadowing-import-from #:system #:define-compiler-macro)
  (:nicknames :cl-macsyma :cl-maxima :macsyma)
  (:import-from :cl-sloop #:sloop)
  (:shadow continue		 ;(macsys): part of the top-level loop
	   //                           ;(clmacs): arithmetic operator
	   float		  ;(clmacs): has 1.0 as default format
	   functionp                    ;(commac): accepts symbols
	   array                        ;(commac)
	   exp			   ;various files declare this special
	   signum                       ;(commac): same except
					; (cl:signum 1.3)==1.0 not 1 but I (?)
					; think this is ok for macsyma
	   atan			;(zl:atan y x) == (cl:atan y x) + 2 pi
					; if latter is negative
	   asin acos asinh acosh atanh  ;different for complex numbers
	   tanh cosh sinh tan  ;(trigi): same, could remove from trigi
	   break		     ; special variable in displa.lisp
	   gcd				; special in rat module
	   ;; (getalias '$lambda) => cl:lambda, which implies that
	   ;; Maxima parses lambda as cl:lambda. 
	   #+(and sbcl sb-package-locks) makunbound)
  #+gcl
  (:import-from :system
		;; Faster modular arithmetic.
		;; Unfortunately, as S. Macrakis observed (bug #706562),
		;; SI::CMOD currently behaves in a strange way:
		;; (let ((si::modulus 4)) (si::cmod 2)) => 2
		;; (let ((si::modulus 4)) (si::cmod -2)) => -2
		#:modulus #:cmod #:ctimes #:cdifference #:cplus

		#:getpid #:get-instream
		#:short-name #:cleanup #:instream-stream-name #:instream-line
		#:instream-name #:instream-stream #:stream-name #:complete-prop
		#:*stream-alist* #:break-call))


(defpackage :mt19937
  (:use :common-lisp)
  (:shadow #:random-state
       #:random-state-p
       #:random
       #:*random-state*
       #:make-random-state)
  (:export #:random-state
       #:random-state-p
       #:random
       #:*random-state*
       #:make-random-state
       #:%random-single-float
       #:%random-double-float
       #+(or scl clisp) #:%random-long-float
       #+cmu #:%random-double-double-float
       #:random-chunk
       #:init-random-state))

;; This package is for the implmentation of the BIGFLOAT routines that
;; make working with Maxima's bfloat objects somewhat easier by
;; extending the standard CL numeric functions to work with BIGFLOAT
;; and COMPLEX-BIGFLOAT objects.  See src/numeric.lisp for the
;; implementation.
(defpackage bigfloat-impl
  (:use :cl)
  #+gcl
  (:shadowing-import-from #:system #:define-compiler-macro)
  (:shadow #:+
	   #:-
	   #:*
	   #:/
	   #:1+
	   #:1-
	   #:zerop
	   #:plusp
	   #:minusp
	   #:abs
	   #:sqrt
	   #:log
	   #:exp
	   #:sin
	   #:cos
	   #:tan
	   #:asin
	   #:acos
	   #:atan
	   #:sinh
	   #:cosh
	   #:tanh
	   #:asinh
	   #:acosh
	   #:atanh
	   #:expt
	   #:=
	   #:/=
	   #:<
	   #:>
	   #:<=
	   #:>=
	   #:scale-float
	   #:realpart
	   #:imagpart
	   #:complex
	   #:conjugate
	   #:max
	   #:min
	   #:cis
	   #:phase
	   #:floor
	   #:ffloor
	   #:incf
	   #:decf
	   #:realp
	   #:complexp
	   #:numberp
	   #:integer-decode-float
	   #:decode-float
	   #:float
	   #:ceiling
	   #:fceiling
	   #:truncate
	   #:ftruncate
	   #:round
	   #:fround
	   #:random
	   #:signum
	   #:float-sign
	   #:float-digits
	   #:rational
	   #:rationalize
	   #:coerce
	   )
  ;; If any of these exported symbols are updated, update the
  ;; shadowing-import-from list for BIGFLOAT-USER too!
  
  ;; Export types
  (:export #:bigfloat
	   #:complex-bigfloat)
  ;; Export functions
  (:export #:bigfloat
	   #:to
	   #:maybe-to
	   #:epsilon
	   #:%pi
	   #:%e
	   ;; CL equivalents
	   #:+
	   #:-
	   #:*
	   #:/
	   #:1+
	   #:1-
	   #:zerop
	   #:plusp
	   #:minusp
	   #:abs
	   #:sqrt
	   #:log
	   #:exp
	   #:sin
	   #:cos
	   #:tan
	   #:asin
	   #:acos
	   #:atan
	   #:sinh
	   #:cosh
	   #:tanh
	   #:asinh
	   #:acosh
	   #:atanh
	   #:expt
	   #:=
	   #:/=
	   #:<
	   #:>
	   #:<=
	   #:>=
	   #:scale-float
	   #:realpart
	   #:imagpart
	   #:complex
	   #:conjugate
	   #:max
	   #:min
	   #:cis
	   #:phase
	   #:floor
	   #:ffloor
	   #:incf
	   #:decf
	   #:realp
	   #:complexp
	   #:numberp
	   #:integer-decode-float
	   #:decode-float
	   #:float
	   #:ceiling
	   #:fceiling
	   #:truncate
	   #:ftruncate
	   #:round
	   #:fround
	   #:random
	   #:signum
	   #:float-sign
	   #:float-digits
	   #:rational
	   #:rationalize
	   #:coerce
	   ))

;; BIGFLOAT is the package intended to be used for applications
;; using the routines from the BIGFLOAT-IMPL.
(defpackage bigfloat
  (:use :cl :bigfloat-impl)
  #+gcl
  (:shadowing-import-from #:system #:define-compiler-macro)
  ;; This list should match the SHADOWING-IMPORT-FROM list in
  ;; BIGFLOAT-IMPL.
  (:shadowing-import-from #:bigfloat-impl
			  #:+
			  #:-
			  #:*
			  #:/
			  #:1+
			  #:1-
			  #:zerop
			  #:plusp
			  #:minusp
			  #:abs
			  #:sqrt
			  #:log
			  #:exp
			  #:sin
			  #:cos
			  #:tan
			  #:asin
			  #:acos
			  #:atan
			  #:sinh
			  #:cosh
			  #:tanh
			  #:asinh
			  #:acosh
			  #:atanh
			  #:expt
			  #:=
			  #:/=
			  #:<
			  #:>
			  #:<=
			  #:>=
			  #:scale-float
			  #:realpart
			  #:imagpart
			  #:complex
			  #:conjugate
			  #:max
			  #:min
			  #:cis
			  #:phase
			  #:floor
			  #:ffloor
			  #:incf
			  #:decf
			  #:realp
			  #:complexp
			  #:numberp
			  #:integer-decode-float
			  #:decode-float
			  #:float
			  #:ceiling
			  #:fceiling
			  #:truncate
			  #:ftruncate
			  #:round
			  #:fround
			  #:random
			  #:signum
			  #:float-sign
			  #:float-digits
			  #:rational
			  #:rationalize
			  #:coerce
			  )
  (:export #:lentz
	   #:sum-power-series))

;; Export all the external symbols in BIGFLOAT-IMPL from BIGFLOAT too.
(do-external-symbols (s '#:bigfloat-impl)
  (export s '#:bigfloat))

;; For CMUCL, we lock the bigfloat-impl package so we don't
;; accidentally modify the implementation.
#+cmu
(defun lock-maxima-packages ()
  (let ((package-names '(#:bigfloat-impl)))
    (dolist (p package-names)
      (let ((p (find-package p)))
	(when p
          (setf (package-definition-lock p) t)
          (setf (package-lock p) t)))))
  (values))


#+cmu
(progn
  (lock-maxima-packages)
  (pushnew 'lock-maxima-packages ext:*after-save-initializations*))


(defpackage :intl
  (:use :common-lisp)
  (:export #:setlocale #:textdomain #:gettext #:dgettext
	   #:ngettext #:dngettext
           #:*translatable-dump-stream* #:*locale*
	   #:*locale-directories*
	   #:read-translatable-string)
  #+gcl
  (:shadowing-import-from #:system #:define-compiler-macro))

(provide :maxima)
