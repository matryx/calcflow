
(defpackage :slatec
  (:use :common-lisp)
  (:export
   ;; Error functions
   #:derf #:derfc

   ;; Bessel function: J
   #:dbesj0 #:dbesj1 #:dbesj #:zbesj

   ;; Bessel function: Y
   #:dbesy0 #:dbesy1 #:dbesy #:zbesy

   ;; Bessel function: I
   #:dbesi0 #:dbesi1 #:dbesi #:dbsi0e #:dbsi1e #:zbesi

   ;; Bessel function: K
   #:dbesk0 #:dbesk1 #:dbesk #:zbesk

   ;; Bessel function: H
   #:zbesh
   
   ;; Airy functions
   #:dai #:zairy #:djairy #:dbi #:zbiry #:dyairy

   ;; Exponential integrals
   #:de1

   ;; Spence's function (related to dilogarithm)
   #:dspenc

   ;; Quadpack routines
   #:dqag #:dqags #:dqagi #:dqawc #:dqawf #:dqawo #:dqaws #:dqagp
   #:j4save
   )
  (:documentation "Package for the Fortran routines we need from SLATEC"))
