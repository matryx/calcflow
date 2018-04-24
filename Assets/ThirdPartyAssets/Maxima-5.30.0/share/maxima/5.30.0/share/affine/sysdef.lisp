(pushnew :cl *features*)

(setf (get :affine :source-path) "foo.lisp")
(setf (get :affine :object-path) "foo.o")

(require "MAKE" "make.lisp")

(setf make::*system-p* nil)

(setf (get :affine :make) '
      (amacros
       (dummy-gc)
       (:progn (proclaim '(optimize (safety 2))))
       polybas
       sparsemat
       (aquotient new-rat)
       polya
       (ndotsimp polyb polysmp sub-proj polyc polyd)
       sheafa sheafb sheafc
       dim-3 ndotsimp modsimp
       todd-coxeter))

(proclaim '(declaration values))

#+kcl
(setf (get 'maxima::fixed-args :proclaim)
      #'(lambda (x)
	  (setf (get x 'compiler::fixed-args) t)))
