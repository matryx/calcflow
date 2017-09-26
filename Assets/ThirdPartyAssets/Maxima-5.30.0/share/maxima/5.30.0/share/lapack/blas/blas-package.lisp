(defpackage :blas
  (:use :common-lisp)
  (:export #:daxpy #:dcopy #:ddot #:dnrm2 #:dscal #:idamax #:dasum
	   #:dcabs1 #:dgbmv #:dgemm #:dgemv #:dger #:drot #:drotg
	   #:dsbmv #:dspmv #:dspr #:dspr2 #:dswap #:dsymm #:dsymv
	   #:dsyr #:dsyr2 #:dsyr2k #:dsyrk #:dtbmv #:dtbsv #:dtpmv
	   #:dtpsv #:dtrmm #:dtrmv #:dtrsm #:dtrsv #:dzasum #:dznrm2
	   #:icamax #:isamax #:izamax #:lsame #:xerbla))
   