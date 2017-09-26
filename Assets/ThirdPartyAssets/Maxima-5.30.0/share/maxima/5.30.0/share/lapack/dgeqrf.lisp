;; dgeqrf.lisp -- Maxima interface to lapack::dgeqrf
;; copyright 2011 by Robert Dodier
;; I release this work under terms of the GNU General Public License.
(in-package :maxima)

;; dgeqrf(a) computes QR factorization of m-by-n Maxima matrix,
;; where m and n might be equal or unequal.
;; a is not modified.

(defun $dgeqrf (a)

  (multiple-value-bind (a-nrow a-ncol)
      (maxima-matrix-dims a)
    (let
      ((a-mat (lapack-lispify-matrix a a-nrow a-ncol))
       (work (make-array 1 :element-type 'flonum))
       (tau (make-array (min a-nrow a-ncol) :element-type 'flonum)))

       ;; First call DGEQRF with LWORK = -1 to find optimal block size NB.
       ;; Use that to compute LWORK.

       (multiple-value-bind (foo-1 foo-2 foo-3 foo-4 foo-5 foo-6 foo-7 foo-8)
         (lapack::dgeqrf a-nrow a-ncol a-mat a-nrow tau work -1 0)
         (declare (ignore foo-1 foo-2 foo-3 foo-4 foo-5 foo-6 foo-7 foo-8))

         (let*
           ((lwork (floor (aref work 0)))
            (work (make-array lwork :element-type 'flonum)))

           (multiple-value-bind (foo-1 foo-2 foo-3 foo-4 foo-5 foo-6 foo-7 z-info)
             (lapack::dgeqrf a-nrow a-ncol a-mat a-nrow tau work lwork 0)
             (declare (ignore foo-1 foo-2 foo-3 foo-4 foo-5 foo-6 foo-7))

             (cond
               ((< z-info 0)
                (merror "dgeqrf: ~M-th argument has an illegal value." (- z-info)))
               (t
                 (dgeqrf-unpack a-nrow a-ncol a-mat tau)))))))))

(defun dgeqrf-unpack (a-nrow a-ncol a-mat tau)
  (let
    ((r (dgeqrf-unpack-r a-nrow a-ncol a-mat))
     (q (dgeqrf-unpack-q a-nrow a-ncol a-mat tau)))
    (list '(mlist) q r)))

(defun dgeqrf-unpack-r (a-nrow a-ncol a-mat)
  (let*
    ((r-nrow a-nrow)
     (r-ncol a-ncol)
     (r-mat (make-array (* r-ncol r-nrow) :element-type 'flonum
					  :initial-element 0e0)))
    (dotimes (j a-ncol)
      (dotimes (i (1+ j))
        (if (< i r-nrow)
          (let
            ((a-ij (+ (* j a-nrow) i))
             (r-ij (+ (* j r-nrow) i)))
            (setf (aref r-mat r-ij) (aref a-mat a-ij))))))
    (lapack-maxify-matrix r-nrow r-ncol r-mat)))

(defun dgeqrf-unpack-q (a-nrow a-ncol a-mat tau)
  (let ((q-mat (make-array (* a-nrow a-nrow) :element-type 'flonum
					     :initial-element 0e0)))
    (dotimes (i a-nrow)
      (setf (aref q-mat (+ (* i a-nrow) i)) 1e0))
    (dotimes (i (min a-nrow a-ncol))
      (let ((h-mat-i (dgeqrf-h i tau a-nrow a-mat)))
        (dgeqrf-multiply-into a-nrow q-mat h-mat-i)))
    (lapack-maxify-matrix a-nrow a-nrow q-mat)))

(defun dgeqrf-h (i tau a-nrow a-mat)
  (let ((h-mat (make-array (* a-nrow a-nrow) :element-type 'flonum)))
    (dotimes (j a-nrow)
      (dotimes (k a-nrow)
        (let*
          ((v-j (dgeqrf-v i j a-nrow a-mat))
           (v-k (dgeqrf-v i k a-nrow a-mat))
           (foo (* (aref tau i) v-j v-k))
           (bar (if (= j k) (1+ (- foo)) (- foo))))
          (setf (aref h-mat (+ (* k a-nrow) j)) bar))))
    h-mat))

(defun dgeqrf-v (i j a-nrow a-mat)
  (cond
    ((= j i) 1)
    ((> j i) (aref a-mat (+ (* i a-nrow) j)))
    (t 0)))

(defun dgeqrf-multiply-into (n mat-1 mat-2)
  (let ((row (make-array n :element-type 'flonum :initial-element 0e0)))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref row j) (dgeqrf-inner-product n mat-1 mat-2 i j)))
      (dotimes (j n)
        (setf (aref mat-1 (+ (* j n) i)) (aref row j))))))

(defun dgeqrf-inner-product (n mat-1 mat-2 i j)
  (let ((sum 0))
    (dotimes (k n)
      (setq sum (+ sum (* (aref mat-1 (+ (* k n) i)) (aref mat-2 (+ (* j n) k))))))
    sum))
