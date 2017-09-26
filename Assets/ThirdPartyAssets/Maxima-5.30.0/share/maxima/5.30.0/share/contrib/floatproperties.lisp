;; Expose some properties of double floating point numbers to Maxima.
;; Note: floatbits is one plus the number of bits in the fractional part.

(defun $float_eps ()
  flonum-epsilon)

(defmvar $largest_float most-positive-flonum)
(setf (get '$largest_float 'assign) 'neverset)

(defmvar $least_positive_float least-positive-flonum)
(setf (get '$least_positive_float 'assign) 'neverset)

(defun $float_bits ()
  (float-digits 0.0))

(defun $bigfloat_eps ()
  (let ((r ($bfloat (div 1 (expt 2 fpprec)))))
    (list (first r) (incf (second r)) (third r))))