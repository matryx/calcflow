;; Load all Lisp files within WITH-COMPILATION-UNIT macro.
;; This quiets the undefined function warnings from SBCL
;; which are otherwise very voluminous (and this construct
;; is accepted by other CL implementations).

#+gcl (defmacro with-compilation-unit (a &rest b) `(progn ,@b))

(with-compilation-unit nil
    ($load "polynomialp")
    ($load "mring")
    ($load "lu")
    ($load "linalgcholesky")
    ($load "eigens-by-jacobi")
    ($load "linalg-extra")
    ($load "matrixexp")
    ($load "linalg-utilities"))

