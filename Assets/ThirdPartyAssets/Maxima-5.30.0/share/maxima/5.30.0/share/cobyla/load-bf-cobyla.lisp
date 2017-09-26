(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+ecl
($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "cobyla" :type "system")
		       #-gcl *load-pathname*
		       #+gcl sys:*load-pathname*))

(mk:oos "bf-cobyla-interface" :compile)
