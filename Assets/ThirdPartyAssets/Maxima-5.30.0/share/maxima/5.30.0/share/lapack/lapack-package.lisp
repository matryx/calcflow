(defpackage #:blas
  (:use #:common-lisp)
  (:export #:daxpy #:dcopy #:ddot #:dnrm2 #:dscal #:idamax #:dasum
	   #:dcabs1 #:dgbmv #:dgemm #:dgemv #:dger #:drot #:drotg
	   #:dsbmv #:dspmv #:dspr #:dspr2 #:dswap #:dsymm #:dsymv
	   #:dsyr #:dsyr2 #:dsyr2k #:dsyrk #:dtbmv #:dtbsv #:dtpmv
	   #:dtpsv #:dtrmm #:dtrmv #:dtrsm #:dtrsv #:dzasum #:dznrm2
	   #:icamax #:isamax #:izamax #:lsame #:xerbla
	   ;; Complex BLAS
	   #:zaxpy
	   #:zcopy
	   #:zdotc
	   #:zdotu
	   #:zdscal
	   #:zgbmv
	   #:zgemm
	   #:zgemv
	   #:zgerc
	   #:zgeru
	   #:zhbmv
	   #:zhemm
	   #:zhemv
	   #:zher
	   #:zher2
	   #:zher2k
	   #:zherk
	   #:zhpmv
	   #:zhpr
	   #:zhpr2
	   #:zrotg
	   #:zscal
	   #:zswap
	   #:zsymm
	   #:zsyr2k
	   #:zsyrk
	   #:ztbmv
	   #:ztbsv
	   #:ztpmv
	   #:ztpsv
	   #:ztrmm
	   #:ztrmv
	   #:ztrsm
	   #:ztrsv
	   )
  (:documentation "Package for BLAS routines"))

(defpackage #:lapack
  (:use :f2cl-lib :blas :common-lisp)
  (:export
   ;; Functions that maxima wants to use from LAPACK.
   #:dgeev
   #:dgesvd
   #:dlange
   #:zlange
   #:zgeev
   )
  (:documentation "Package for LAPACK routines"))
