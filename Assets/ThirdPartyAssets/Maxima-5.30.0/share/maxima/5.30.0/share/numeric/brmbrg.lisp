;;; -*- Lisp -*-

(defvar $brombergit 11) 
(defvar $brombergmin 0)
(defvar $brombergtol '((bigfloat simp 56.) 59029581035870565. -13.)) ;1.b-4
(defvar $brombergabs '((bigfloat simp 56.) 0 0)) ; 0.0b0

(defun fpscale (x m)
  (if (equal (car x) 0)
      x
      (list (car x) (+ (cadr x) m))))

(defun bfmeval3 (x1) 
  (cond (($bfloatp (setq x1 ($bfloat (meval x1)))) (cdr x1))
	(t (displa x1) (merror "bromberg: encountered a non-bigfloat."))))

(defun bqeval3 (f x)
  (cdr (funcall f (bcons x))))

(defun $bromberg (&rest l1) 
  (or (= (length l1) 4) (= (length l1) 3)
      (merror "bromberg: wrong number of arguments."))
  (let ((fun) (var) (a) (b) (x)
	($bfloat t)
	($float2bf t)
	(lim (cdr ($bfloat $brombergtol)))
	(limabs (cdr ($bfloat $brombergabs)))
	(tt (make-array $brombergit))
	(rr (make-array $brombergit))
	(zero (intofp 0))
	(one (intofp 1))
	(three (intofp 3)))
    (declare (special $bfloat $float2bf))
    (setq var (= (length l1) 4)) ;var=nil ==> first arg is function name 
    (cond (var (setq var (cadr l1)
                     fun (coerce-bfloat-fun (car l1) `((mlist) ,var))
		     l1 (cdr l1)))
	  (t (setq fun (coerce-bfloat-fun (car l1)))))
    (setq a (bfmeval3 (cadr l1)) 
	  b (bfmeval3 (caddr l1))
	  x (fpdifference b a))
    (setf (aref tt 0)
	  (fpscale (fptimes* x (fpplus (bqeval3 fun b)
				       (bqeval3 fun a)))
		   -1))
    (setf (aref rr 0) (fptimes* x (bqeval3 fun (fpscale (fpplus b a) -1))))
    (do ((l 1 (1+ l))
	 (m 4 (* m 2))
	 (y) (z) (cerr))
	((= l $brombergit) (merror "bromberg: failed to converge."))
      (setq y (intofp m) z (fpquotient x y))
      (setf (aref tt l)
	    (fpscale (fpplus (aref tt (1- l)) (aref rr (1- l))) -1))
      (setf (aref rr l) zero)
      (do ((i 1 (+ i 2)))
	  ((> i m))
	(setf (aref rr l)
	      (fpplus (bqeval3 fun (fpplus (fptimes* z (intofp i)) a))
		      (aref rr l))))
      (setf (aref rr l) (fpscale (fptimes* z (aref rr l)) 1))
      (setq y zero)
      (do ((k l (1- k)))
	  ((zerop k))
	(setq y (fpplus (fpscale y 2) three))
	(setf (aref tt (1- k))
	      (fpplus (fpquotient
		       (fpdifference (aref tt k) (aref tt (1- k)))
		       y)
		      (aref tt k)))
	(setf (aref rr (1- k))
	      (fpplus (fpquotient
		       (fpdifference (aref rr k) (aref rr (1- k)))
		       y)
		      (aref rr k))))
      (setq y (fpscale (fpplus (aref tt 0) (aref rr 0)) -1))
      (when (and
	     (or (not
		  (fplessp
		   limabs
		   (setq cerr
			 (fpabs (fpdifference (aref tt 0)
					      (aref rr 0))))))
		 (not (fplessp lim
			       (fpquotient
				cerr
				(cond ((equal y '(0 0)) one)
				      (t (fpabs y)))))))
	     (> l $brombergmin))
	(return (bcons y)))))) 
