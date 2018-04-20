;; f2cl0.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :common-lisp-user)

(defpackage :f2cl-lib
  (:use :cl)
  (:documentation "The package holding all symbols used by the Fortran to Lisp library")
  (:nicknames :fortran-to-lisp-library)
  (:export
   ;; Constants
   #:%false% #:%true%
   ;; User-settable runtime options
   #:*check-array-bounds*
   #:*stop-signals-error-p*
   ;; Types
   #:integer4 #:integer2 #:integer1 #:real8 #:real4 #:complex8 #:complex16
   #:array-double-float #:array-single-float #:array-integer4 #:array-strings
   #:logical
   ;; Macros
   #:fref #:fset #:with-array-data
   #:with-multi-array-data
   #:f2cl-init-string #:fref-string #:fset-string #:f2cl-set-string
   #:f2cl-// #:fstring-/= #:fstring-= #:fstring-> #:fstring->= #:fstring-< #:fstring-<=
   #:fortran_comment #:fdo #:f2cl/ #:arithmetic-if #:computed-goto
   #:assigned-goto
   #:fformat
   #:data-implied-do
   #:int-add #:int-sub #:int-mul
   ;; Utilities
   #:array-slice #:array-initialize
   ;; Intrinsic functions
   #:abs #:acos #:aimag #:dimag #:aint #:alog #:alog10 #:amax0 #:amax1
   #:amin1 #:amod #:anint #:asin #:atan #:atan2
   #:cabs #:cexp #:fchar #:clog #:cmplx #:dcmplx #:conjg #:ccos
   #:csin #:csqrt #:zsqrt #:dabs #:dacos #:dasin
   #:datan #:datan2 #:dble #:dcos #:dcosh #:dexp #:dfloat #:dim
   #:dint #:dlog #:dlog10 #:dmax1 #:dmin1 #:dmod
   #:dnint #:dprod #:dsign #:dsin #:dsinh #:dsqrt #:dtan
   #:dtanh #:ffloat #:iabs #:ichar #:idim #:idint
   #:idnint #:ifix #:index #:int #:isign #:le #:len
   #:lge #:lgt #:flog #:log10 #:lt #:max #:max0
   #:max1 #:min0 #:min1 #:nint #:freal
   #:sign #:sngl #:fsqrt
   #:cdabs #:dconjg
   ;; other functions
   #:d1mach #:r1mach #:i1mach
   ))

#+nil
(defpackage :fortran-to-lisp
  (:use :cl)
  (:documentation "The package holding all symbols need by the Fortran to Lisp converter")
  (:nicknames :f2cl)
  (:export
   ;; Main routines
   #:f2cl
   #:f2cl-compile
   #:f2cl-version
   ))

;;;-------------------------------------------------------------------------
;;; end of f2cl0.l
;;;
;;; $Id: f2cl-package.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $
;;; $Log: f2cl-package.lisp,v $
;;; Revision 1.11  2009-01-08 18:25:34  rtoy
;;; Update f2cl to latest version of f2cl, and regenerate all of the lisp
;;; code.
;;;
;;; The testsuite works fine, so I assume the quadpack and other slatec
;;; routines are ok.
;;;
;;; The lsquares tests runs fine, so the lbfgs routines appear to be ok.
;;;
;;; src/numerical/f2cl-lib.lisp:
;;; o Update from f2cl macros.l, 2009/01/08
;;;
;;; src/numerical/f2cl-package.lisp:
;;; o Update from f2cl f2cl0.l, 2009/01/08
;;;
;;; src/numerical/slatec:
;;; o Regenerate lisp files
;;;
;;; share/lbfgs:
;;; o Split lbfgs.f into one function per file and add these new files.
;;; o Generate new lisp files
;;; o Update lbfgs.mac to load the new list files.
;;; o Added lbfgs-lisp.system so we know how to regenerate the lisp files
;;;   in the future.
;;;
;;; share/lapack:
;;; o Add lapack-lisp.system so we know how to regenerate the lisp files
;;;   in the future.
;;; o Regenerate all of the lisp files.
;;;
;;; Revision 1.10  2007/04/07 19:09:01  dtc
;;; o Fix some symbol case issues.  This enables the Lapack code to run in a
;;;   lowercase Common Lisp variant.
;;;
;;; Revision 1.9  2006/12/20 18:13:05  rtoy
;;; Update to latest f2cl versions.
;;;
;;; Revision 1.8  2005/11/07 17:37:12  rtoy
;;; This large set of changes comes from Douglas Crosher adding support
;;; for SCL:
;;;
;;; o Change package names to use keywords and uninterned symbols instead
;;;   of strings, so SCL (and probably Allegro) can use lower-case mode.
;;; o Downcase a few uppercase symbols that were inadvertently left out in
;;;   the great downcasing.
;;; o Add support for building Maxima with SCL
;;;
;;; SCL support is untested.
;;;
;;; Revision 1.7  2004/10/19 12:05:00  wjenkner
;;; Eliminate all references to LISP or USER in Maxima, replacing them by
;;; CL and CL-USER, respectively (except for system-dependent stuff, like
;;; lisp:bye, of course).  In particular, make MAXIMA inherit from CL,
;;; not from LISP (take a look at the cleaned-up maxima-package.lisp).
;;; Also, all lisps now use the ERRSET from src/generr.lisp, so that
;;; src/kclmac.lisp with the SERROR stuff can be eliminated.
;;;
;;; Other changes needed for this:
;;;
;;; Avoid package locking errors by obeying to the CLHS 11.1.2.1.2
;;; (Constraints on the COMMON-LISP Package for Conforming Programs).
;;;
;;; lmdcls.lisp, mdebug.lisp, merror.lisp, mlisp.lisp, suprv1.lisp: Replace
;;; the special variable DEBUG by *MDEBUG*.
;;;
;;; commac.lisp, mtrace.lisp: Replace PRIN1 as special variable by *PRIN1*.
;;;
;;; specfn.lisp: Replace LAST as special variable by *LAST*
;;;
;;; maxima-package.lisp: Shadow GCD and BREAK.  For SBCL, shadow
;;; MAKUNBOUND in order to avoid package locking errors at runtime.
;;;
;;; commac.lisp: Give GCD and BREAK their CL function definition.  For
;;; SBCL, redefine MAKUNBOUND.
;;;
;;; db.lisp: Boldly replace the special variable * by db*
;;;
;;; limit.lisp ($limit): Don't declare T special.
;;;
;;; Revision 1.6  2004/10/04 02:25:54  amundson
;;; Downcasing of source complete. Code compiles and passes tests with
;;; clisp, cmucl, gcl and sbcl.
;;;
;;; Revision 1.5  2003/11/26 17:27:16  rtoy
;;; Synchronize to the current versions of f2cl.
;;;
;;; Revision 1.4  2002/07/18 02:37:22  rtoy
;;; Don't need to import destructuring-bind for GCL anymore.
;;;
;;; Revision 1.3  2002/05/19 20:22:32  rtoy
;;; GCL doesn't export DESTRUCTURING-BIND from the LISP package.  Import
;;; it.
;;;
;;; Revision 1.2  2002/05/01 18:20:18  amundson
;;; Package-related hacks for gcl.
;;;
;;; Revision 1.1  2002/04/26 13:04:46  rtoy
;;; Initial revision.
;;;
;;; Revision 1.2  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-------------------------------------------------------------------------
