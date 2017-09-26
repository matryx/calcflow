(in-package :maxima)

(load (combine-path *maxima-sharedir* "affine" "affine.system"))

(mk:compile-system "affine" :load-source-if-no-binary t)

;;; affine.lisp ends here
