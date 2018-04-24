(declare (special $floatformat floatmax floatmin floatsmall
		  floatbig floatbigbig float-enote))


(defmvar $floatformat t)

;;; defaults

(defmvar floatmax 6)
(defmvar floatmin -4)
(defmvar floatbig 2)
(defmvar floatbigbig 1)
(defmvar floatsmall 3)
(defmvar float-enote 2)

(putprop 'makestring1 (get 'makestring 'subr) 'subr)

(defun makestring (form)
       (cond ((and $floatformat (floatp form)) (nicefloat form))
	     ((makestring1 form))))

(defun nicefloat (flt)
  (cond ((= flt 0.0) (list 48. 46. 48.))
	((< flt 0.0) (cons 45. (niceflt (abs flt))))
	((niceflt (abs flt)))))

(defun niceflt (aflt)
  (declare (fixnum i))
  (do ((i 0)
       (simflt aflt)
       (fac (cond ((< aflt 1.0) 1e1) (1e-1)))
       (inc (cond ((< aflt 1.0) -1) (1))))
      ((and (< simflt 1e1) (not (< simflt 1.0)))
       (floatcheck (exploden simflt) i))
    (setq simflt (* simflt fac))
    (incf i inc)))

(defun floatcheck (repres pwr)
    (declare (fixnum pwr))
    (cond
      ((or (> pwr (1- floatmax)) (< pwr floatmin))
       (cons (car repres)
	     (cons 46.
		   (append (fracgen (cddr repres) float-enote nil)
			   (cons 69.(cond ((> pwr 0)
					   (cons 43 (exploden pwr)))
					  ((exploden pwr))))))))
      ((< pwr 0.)
       ((lambda (frac)
	  (cons 48.
		(cons 46.
		      (cond ((equal frac '(48.)) frac)
			    ((append (fraczeros (1- (abs pwr)))
				     frac))))))
	(fracgen (delete 46. repres) floatsmall nil)))
      ((cons (car repres)
	     (floatnone (cddr repres)
			pwr
			(cond ((< pwr 3.) floatbig)
			      (floatbigbig)))))))

(defun fraczeros (n)
       (declare (fixnum n))
       (cond ((zerop n) nil) ((cons 48. (fraczeros (1- n))))))

(defun floatnone (repres pwr floatfrac)
       (declare (fixnum pwr floatfrac))
       (cond ((zerop pwr) (cons 46. (fracgen repres floatfrac nil)))
	     ((cons (cond (repres (car repres)) (48.))
		    (floatnone (cdr repres) (1- pwr) floatfrac)))))

(defun felimin (revrep)
       (cond ((null revrep) (ncons 48.))
	     ((= (car revrep) 48.) (felimin (cdr revrep)))
	     ((reverse revrep))))

(defun fracgen (repres floatfrac result)
       (declare (fixnum floatfrac))
       (cond ((null repres) (felimin result))
	     ((zerop floatfrac) (felimin result))
	     ((fracgen (cdr repres)
		       (1- floatfrac)
		       (cons (car repres) result)))))
