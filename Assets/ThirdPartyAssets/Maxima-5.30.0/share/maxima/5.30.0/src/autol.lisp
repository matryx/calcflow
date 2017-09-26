(in-package :maxima)

;; These are the helper functions for autoloading.
;; The actual autoloading data is in src/max_ext.lisp
;;(aload "plot.o")

(defun aload (file &aux *load-verbose* tem)
  (let ((*read-base* 10.)
	($system  (list  '(mlist)
			 #+kcl (concatenate 'string si::*system-directory*
					    "../src/foo.{o,lsp,lisp}"))))
    (declare (special $system))
    (setq tem ($file_search1 file '((mlist)
				    $file_search_lisp
				    $system)))
    (and tem #-sbcl (load tem) #+sbcl (with-compilation-unit nil (load tem)))))

(defun $aload_mac (file &aux *load-verbose* tem)
  (let (($system  (list  '(mlist)
			 #+kcl (concatenate 'string si::*system-directory*
					    "../{src,share,share1,sharem}/foo.{mc,mac}"))))
    (declare (special $system))
    (setq tem ($file_search1 file '((mlist)
				    $file_search_maxima
				    $system)))
    (and tem ($load tem))))


;;for defun,defmfun
(defun autof (fun file)
  (unless (fboundp fun)
    (setf (symbol-function fun)
     	  #'(lambda (&rest l)
	      (aload file)
	      (apply fun l)))))

;;for defmacro
(defun autom (fun file)
  (unless (fboundp fun)
    (setf (macro-function fun)
	  #'(lambda (&rest l)
	      (aload file)
	      (funcall (macro-function fun)
		       (cons fun l) nil)))))
;;for defmspec
(defun auto-mspec (fun file )
  (unless (get fun 'mfexpr*)
    (setf (get fun 'mfexpr*)
	  #'(lambda (l)
	      (aload file)
	      (funcall (get fun 'mfexpr*) l)))))

;;foo(x,y):=..
(defun auto-mexpr (fun file)
  (unless (mget fun 'mexpr)
    (mputprop fun
              `((lambda) ((mlist) ((mlist) |_l|))
		((mprogn) ((aload) ((mquote) ,file)) (($apply) ((mquote) ,fun) |_l|)))
	      'mexpr)))

;;foo(x,y):=..
(defun $auto_mexpr (fun file)
  (unless (mget fun 'mexpr)
    (mputprop fun
              `((lambda) ((mlist) ((mlist) |_l|))
		((mprogn) (($aload_mac) ((mquote) ,file)) (($apply) ((mquote) ,fun) |_l|)))
	      'mexpr)))
