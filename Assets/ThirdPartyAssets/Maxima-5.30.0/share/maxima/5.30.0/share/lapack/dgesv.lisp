;; dgesv.lisp -- Maxima interface to lapack::dgesv
;; copyright 2010 by Robert Dodier
;; I release this work under terms of the GNU General Public License.
(in-package :maxima)

;; dgesv(a, b) returns solution x of linear equations a . x = b
;; as computed by the LU decomposition.
;; a is a n-by-n Maxima matrix, b is a n-by-m Maxima matrix,
;; where m maybe be greater than or equal to 1.
;; a and b are not modified.

(defun $dgesv (a b)

  (multiple-value-bind (a-nrow a-ncol) (maxima-matrix-dims a)
    (multiple-value-bind (b-nrow b-ncol) (maxima-matrix-dims b)

      (let
        ((a-mat (lapack-lispify-matrix a a-nrow a-ncol))
         (b-mat (lapack-lispify-matrix b b-nrow b-ncol))
         (ipiv (make-array a-nrow :element-type 'f2cl-lib:integer4)))

        (multiple-value-bind (z-n z-nrhs z-a z-lda z-ipiv z-b z-ldb$ z-info)
          (lapack::dgesv a-nrow b-ncol a-mat a-nrow ipiv b-mat b-nrow 0)

          (declare (ignore z-n z-nrhs z-a z-lda z-ipiv z-b z-ldb$))
          (cond
            ((< z-info 0)
             (merror "dgesv: ~M-th argument has an illegal value." (- z-info)))
            ((> z-info 0)
             (merror "dgesv: U(~M, ~M) is exactly zero; cannot compute a solution." z-info z-info))
            (t
              (lapack-maxify-matrix b-nrow b-ncol b-mat))))))))
