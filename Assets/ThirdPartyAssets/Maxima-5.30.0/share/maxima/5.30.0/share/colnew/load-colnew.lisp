#+nil
(format t "colnew.system = ~S~%" (merge-pathnames (make-pathname :name "colnew" :type "system")
						  #-gcl *load-pathname*
						  #+gcl sys:*load-pathname*))
(load (merge-pathnames (make-pathname :name "colnew" :type "system")
		       #-gcl *load-pathname*
		       #+gcl sys:*load-pathname*))

(mk:oos "colnew-if" :compile)