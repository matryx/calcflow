; Copyright 2006 by Robert Dodier
; Released under the terms of the GNU General Public License.

; Example 1:
; This is the same FOM as computed by FGCOMPUTE in the program sdrive.f
#|
load (lbfgs);
t1[j] := 1 - u[j];
t2[j] := 10*(u[j + 1] - u[j]^2);
n : 8;
FOM : sum (t1[2*j - 1]^2 + t2[2*j - 1]^2, j, 1, n/2);
lbfgs_nfeval_max : 100;
lbfgs_ncorrections : 25;
lbfgs (FOM, '[u[1], u[2], u[3], u[4], u[5], u[6], u[7], u[8]],
       [-1.2, 1, -1.2, 1, -1.2, 1, -1.2, 1], 1e-4, [1, 3]);

  => [u[1] = 1.00000047321587,u[2] = 1.000000931806471,
      u[3] = 1.00000047321587,u[4] = 1.000000931806471,
      u[5] = 1.00000047321587,u[6] = 1.000000931806471,
      u[7] = 1.00000047321587,u[8] = 1.000000931806471]$

 |#

; Example 2:
; This computes the least-squares fit to a function A/(1 + exp(-B * (x - C)))
#|
load (lbfgs);
FOM : '((1/length(X))*sum((F(X[i]) - Y[i])^2, i, 1, length(X)));
X : [1, 2, 3, 4, 5];
Y : [0, 0.5, 1, 1.25, 1.5];
F(x) := A/(1 + exp(-B*(x - C)));
''FOM;
estimates : lbfgs (FOM, '[A, B, C], [1, 1, 1], 1e-4, [1, 3]);
plot2d ([F(x), [discrete, X, Y]], [x, -1, 6]), ''estimates;
 |#

; estimates => [A = 1.461933911464101, B = 1.601593973254801, C = 2.528933072164855]

; Example 3:
; Gradient of FOM specified directly (instead of constructing it via diff)
#|
load (lbfgs);
F(a, b, c) := (a - 5)^2 + (b - 3)^4 + (c - 2)^6;
F_grad : map (lambda ([x], diff (F(a, b, c), x)), [a, b, c]);
estimates : lbfgs ([F(a, b, c), F_grad], [a, b, c], [0, 0, 0], 1e-4, [1, 0]);
 |#

; estimates => [a = 5.000086823042934, b = 3.052395429705181, c = 1.927980629919583]

; Example 4:
; same as Example 3, but FOM and gradient are computed by Lisp functions
; Construct Lisp functions suitable for this example via translate
#|
load (lbfgs);
F(a, b, c) := (a - 5)^2 + (b - 3)^4 + (c - 2)^6;
F1(a, b, c) := ''(diff(F(a, b, c), a));
F2(a, b, c) := ''(diff(F(a, b, c), b));
F3(a, b, c) := ''(diff(F(a, b, c), c));
translate (F, F1, F2, F3);
:lisp (mremprop '|$f| 'mexpr)
:lisp (mremprop '|$f1| 'mexpr)
:lisp (mremprop '|$f2| 'mexpr)
:lisp (mremprop '|$f3| 'mexpr)
estimates : lbfgs ('[F(a, b, c), [F1(a, b, c), F2(a, b, c), F3(a, b, c)]],
                   [a, b, c], [0, 0, 0], 1e-4, [1, 0]);
 |#

; estimates => [a = 5.000086823042934, b = 3.052395429705181, c = 1.927980629919583]

(in-package :maxima)

(defmvar $lbfgs_nfeval_max 100)
(defmvar $lbfgs_ncorrections 25)

(defmfun $lbfgs (FOM-expr x-list x-initial eps iprint-list)

  (if
    (or (and (symbolp FOM-expr) (mfboundp FOM-expr))
        (and (consp FOM-expr) (eq (caar FOM-expr) 'lambda)))
    (merror "lbfgs: figure of merit, if specified alone, cannot be a function name or lambda."))

  (let*
    ((n (length (cdr x-list)))
     (m $lbfgs_ncorrections)
     (nwork (+ (* n (+ (* 2 m) 1)) (* 2 m)))

     (xtol flonum-epsilon)
     (iflag 0)
      
     (scache (make-array n :element-type 'flonum))
     (w (make-array nwork :element-type 'flonum))
     (diag (make-array n :element-type 'flonum))
     (g (make-array n :element-type 'flonum))
     (x (make-array n :element-type 'flonum))
     (iprint (make-array 2 :element-type 'f2cl-lib:integer4))
     (diagco nil)
     
     (return-value '((mlist)))
     (f 0.0)

     FOM-function FOM-grad-lambda FOM-grad-expr FOM-grad-function)
    
    (if ($listp FOM-expr)
      (progn
        (setq FOM-function (coerce-float-fun ($first FOM-expr) x-list))
        (setq FOM-grad-expr ($second FOM-expr))
        (setq FOM-grad-function (coerce-float-fun FOM-grad-expr x-list)))
      (progn
        (setq FOM-function (coerce-float-fun FOM-expr x-list))
        (setq FOM-grad-lambda `(lambda (x) (meval (list '($diff) ',FOM-expr x))))
        (setq FOM-grad-expr `((mlist) ,@(mapcar (coerce FOM-grad-lambda 'function) (cdr x-list))))
        (setq FOM-grad-function (coerce-float-fun FOM-grad-expr x-list))))

    (setf (aref iprint 0) (nth 1 iprint-list))
    (setf (aref iprint 1) (nth 2 iprint-list))
    (setf diagco f2cl-lib:%false%)

    (let (($numer t)) (setq x-initial ($float (meval x-initial))))
    ;; Fill X from the initial value, skipping over the Maxima mlist marker.
    (replace x x-initial :start2 1)

    (common-lisp-user::/blockdata-lb2/)

    (dotimes (nfeval $lbfgs_nfeval_max)
; (format t "hey nfeval ~S~%" nfeval)
; (format t "hey x ~S~%" x)
      (setf f (apply 'funcall `(,FOM-function ,@(coerce x 'list))))
      (let ((g-list (apply 'funcall `(,FOM-grad-function ,@(coerce x 'list)))))
; (format t "hey g-list ~S~%" g-list)
	(when (eq t g-list)
	  (merror "Evaluation of gradient at ~M failed.  Bad initial point?~%"
		  (list* '(mlist) (coerce x 'list))))
	;; Copy the answer from the grad function to the g array,
	;; skipping over the mlist marker.
	(replace g g-list :start2 1))
; (format t "hey f ~S g ~S~%" f g)

      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                  var-8 var-9 var-10 var-11 var-12)
        (common-lisp-user::lbfgs n m x f g diagco diag iprint eps xtol w iflag scache)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                         var-8 var-9 var-10 var-12))
        (setf iflag var-11)
        (cond
          ; MIGHT WANT TO RETURN SCACHE VALUES UNCONDITIONALLY
          ; (RESULT OF MOST RECENT LINE SEARCH IS BETTER THAN NOTHING)
          ((eq iflag 0)
           (setq return-value (append '((mlist)) (mapcar #'(lambda (a b) `((mequal) ,a ,b)) (cdr x-list) (coerce scache 'list))))
           (return)))))

    return-value))
