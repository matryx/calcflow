;;; -*-  Mode: Lisp -*-                                                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;;;                                                                   ;;; ;;
;; ;;;                        ~*~ SIMPLEX ~*~                            ;;; ;;
;; ;;;                                                                   ;;; ;;
;; ;;;               A simple implementation of the simplex              ;;; ;;
;; ;;;             algorithm for Linear Programming for Maxima.          ;;; ;;
;; ;;;                                                                   ;;; ;;
;; ;;;                                                                   ;;; ;;
;; ;;;   Copyright:  Andrej Vodopivec <andrejv@users.sourceforge.net>    ;;; ;;
;; ;;;   Version:    1.02                                                ;;; ;;
;; ;;;   License:    GPL                                                 ;;; ;;
;; ;;;                                                                   ;;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                           ;;
;;                                                                           ;;
;; USAGE:                                                                    ;;
;; =======                                                                   ;;
;;                                                                           ;;
;; To get optimal speed this file should be compiled.                        ;;
;;                                                                           ;;
;; linear_program(A, b, c):                                                  ;;
;;                                                                           ;;
;;  - problem: - find x which minimizes c'.x with constraints A.x=b and x>=0 ;;
;;                                                                           ;;
;;  - input:   - matrix A of size mxn,                                       ;;
;;             - vector (list) b of length m,                                ;;
;;             - vector (list) c of length n                                 ;;
;;                                                                           ;;
;;  - output:  - [x, val] if the problem is solvable;                        ;;
;;               x is the optimal vector, val=c'.x                           ;;
;;             - "Problem not bounded" if the problem is not bounded         ;;
;;             - "Problem not feasible" if the problem is not feasible       ;;
;;                                                                           ;;
;;  - algorithm: a simple implementation of the two-phase simplex algorithm  ;;
;;                                                                           ;;
;;                                                                           ;;
;; DEMO:                                                                     ;;
;; ======                                                                    ;;
;;                                                                           ;;
;; We would like to minimize x-2*y subject to:                               ;;
;;                                                                           ;;
;;       x +   y >= 1                                                        ;;
;;     2*x - 3*y >= 1                                                        ;;
;;     4*x - 5*y  = 6                                                        ;;
;;       x,    y >= 0                                                        ;;
;;                                                                           ;;
;; We have to introduce two slack variables for inequalities to get a        ;;
;; linear program in the standard form:                                      ;;
;;                                                                           ;;
;;       x +   y - s1       = 1                                              ;;
;;     2*x - 3*y      - s2  = 1                                              ;;
;;     4*x - 5*y            = 6                                              ;;
;;       x,    y,  s1,  s2 >= 0                                              ;;
;;                                                                           ;;
;; Construct parameters:                                                     ;;
;;                                                                           ;;
;;  A : matrix([1,1,-1,0],[2,-3,0,-1], [4,-5,0,0]);                          ;;
;;  b : [1,1,6];                                                             ;;
;;  c : [1,-2,0,0];                                                          ;;
;;                                                                           ;;
;; Solution:                                                                 ;;
;;                                                                           ;;
;;  linear_program(A, b, c);                                                 ;;
;;  => [[13/2, 4, 19/2, 0], -3/2]                                            ;;
;;                                                                           ;;
;; The solution is: x=13/2 and y=4 (s1=19/2, s2=0), and the value of the     ;;
;; minimum is x-2*y=-3/2.                                                    ;;
;;                                                                           ;;
;;                                                                           ;;
;; VARIABLES:                                                                ;;
;; ===========                                                               ;;
;;                                                                           ;;
;; - pivot_count_sx: the number of pivots in last computation                ;;
;; - pivot_max_sx:   the maximum number of pivots in computation             ;;
;; - epsilon_lp:     epsilon for numeric computation (default: 1e-8)         ;;
;; - scale_lp:       should maxima scale the problem: can be used in         ;;
;;                   Klee-Minty problem to speed-up computation or in some   ;;
;;                   cases to improve numerical stability (default: false);  ;;
;;                   uses equilibratium scaling                              ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module simplex)

($put '$simplex 1.02 '$version)

(defmvar $pivot_count_sx     0  "Number of pivots in last problem."   fixnum)
(defmvar $pivot_max_sx   15000  "Maximum number of pivots allowed."   fixnum)
(defmvar $epsilon_lp      1e-8  "Epsilon for numerical computation."  flonum)
(defmvar $scale_lp         nil  "Should we scale the input."         boolean)
(defmvar $warn_rank_sx     nil  "Print warnings about rank."         boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Two-phase standard simplex method for solving linear program in standard  ;;
;; form.                                                                     ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun $linear_program (A b c)
  (if (not ($matrixp A))
      (merror "linear_program: first argument not matrix."))
  (if (not ($listp b))
      (merror "linear_program: second argument not list."))
  (if (not ($listp c))
      (merror "linear_program: third argument not list."))
  (if (not (meqp ($length b) ($length A)))
      (merror "linear_program: second argument not of correct length."))
  (if (not (meqp ($length c) ($length ($first A))))
      (merror "linear_program: third argument not of correct length."))
  
  (let* ((m ($length A))
         (n ($length ($first A)))
         (Tab (make-array `(,(+ 2 m) ,(1+ n)))) ; Tableau
         (basis ())      ; which columns are in current basis
         (sc-fac ())     ; scaling factors
	 ($ratprint nil))
    
    (setq $pivot_count_sx 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construct the tableau for phase 1:                              ;;
;;       [   A      b    ]                                         ;;
;; Tab = [   c'     0    ]                                         ;;
;;       [ sm(A)  sm(b)  ]                                         ;;
;;                                                                 ;;
;; If b[i]<0 multiply b[i] and A[i] by -1 (required for phase 1).  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (dotimes (i m)
      (if (mlsp (maref b (1+ i)) 0)
          (progn
            (dotimes (j n)
              (setf (aref Tab i j) (neg (maref A (1+ i) (1+ j)))))
            (setf (aref Tab i n) (neg (maref b (1+ i)))))
          (progn
            (dotimes (j n)
              (setf (aref Tab i j) (maref A (1+ i) (1+ j))))
            (setf (aref Tab i n) (maref b (1+ i))))))
    (dotimes (i n)
      (setf (aref Tab m i) (neg (maref c (1+ i)))))
    (dotimes (i n)
      (setf (aref Tab (1+ m) i) 0)
      (dotimes (j m)
        (setf (aref Tab (1+ m) i) (add (aref Tab (1+ m) i)
                                       (aref Tab j i)))))
    (setf (aref Tab (1+ m) n) 0)
    (dotimes (i m)
      (setf (aref Tab (1+ m) n) (add (aref Tab (1+ m) n)
                                     (aref Tab i n))))
    (setf (aref Tab m n) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At the beginning the artificial variables are in the basis.     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (dotimes (i m)
      (setq basis (append basis (list (add n i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scaling.                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (dotimes (i n)
      (setq sc-fac (append sc-fac (list 1))))
    (if $scale_lp
        (scale-sx Tab (+ 2 m) (1+ n) sc-fac))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Phase 1                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (simplex-sx Tab basis m (+ 2 m) (1+ n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (if (mlsp $epsilon_lp (aref Tab (1+ m) n))
        "Problem not feasible!"
        (let ((is-bounded))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for artificial variables in basis.                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (dotimes (i m)
            (if (>= (nth i basis) n)
                (if (and (not (run-out-of-basis-sx Tab m n basis i))
			 $warn_rank_sx)
                    ($print "Matrix A is not of full rank:"
                            "Row" (1+ i) "is redundant!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Phase 2                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (setq is-bounded (simplex-sx Tab basis m (1+ m) (1+ n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (if is-bounded
              (let ((opt-tmp ())
                    (opt ()))
                (dotimes (i (+ m n))
                  (setq opt-tmp (append opt-tmp (list 0))))
                (dotimes (i m)
                  (setf (nth (nth i basis) opt-tmp)
                        (div (aref Tab i n)                ;; undo the
                             (nth (nth i basis) sc-fac)))) ;; scaling
                (dotimes (i n)
                  (setq opt (append opt (list 0))))
                (dotimes (i n)
                  (setf (nth i opt) (nth i opt-tmp)))
                (setq opt (cons '(mlist simp) opt))        ;; build the
                (setq opt `((mlist simp) ,opt              ;; the solution
                            ,(aref Tab m n))))
              "Problem not bounded!")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Simplex algorithm.                                                        ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simplex-sx (Tab basis Am m n)
  (let ((ip) (jp) (tmp)  (is-bounded))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Repeat while we don't have a solution or know that the        ;;
  ;; problem is unbounded.                                         ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (do ((have-solution nil))
        ((not (null have-solution)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Choose jp so that jp-th column has the biggest reduced cost.  ;;
  ;; If all reduced costs are negative, we have the solution.      ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq tmp (aref Tab (1- m) 0))
      (setq jp 0)
      (dotimes (j (- n 1))
        (if (mlsp tmp (aref Tab (1- m) j))
            (progn
              (setq tmp (aref Tab (1- m) j))
              (setq jp j))))
      (if (mgqp $epsilon_lp tmp)
          (progn
            (setq is-bounded t)
            (setq have-solution t))
          (progn
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Choose ip so that Tab[ip,n]/Tab[ip,jp] is the smallest        ;;
  ;; possible among those for which Tab[i,n] is positive. If all   ;;
  ;; Tab[i,n] are negative, the problem is unbounded.              ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (setq tmp nil)
            (setq ip 0)
            (dotimes (i Am)
              (if (mlsp $epsilon_lp (aref Tab i jp))
                  (if (or (null tmp) (mlsp (div (aref Tab i (1- n))
                                                (aref Tab i jp))
                                           tmp))
                      (progn
                        (setq tmp (div (aref Tab i (1- n))
                                       (aref Tab i jp)))
                        (setq ip i)))))
            (if (null tmp)
                (progn
                  (setq is-bounded nil)
                  (setq have-solution t))
                (progn
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Pivot the simplex tableau.                                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  (setf (nth ip basis) jp)
                  (pivot-sx Tab ip jp m n))))))
    is-bounded))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Pivoting for the Simplex algorithm.                                       ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pivot-sx (Tab ip jp m n)
  (let ((piv (aref Tab ip jp)))
    (setq $pivot_count_sx (1+ $pivot_count_sx))
    (if (meqp $pivot_count_sx $pivot_max_sx)
        (progn
          ($print "Maximum number of pivots reached.")
          ($print "Try setting a bigger value for pivot_max_sx.")
          ($print "Try setting scale_lp to true.")
          ($print "")
          ($error "linear_program: maximum number of pivots reached.")))
    (if (meqp piv 0)
        ($print "Singular!")
        (progn
          (dotimes (i n)
            (setf (aref Tab ip i) (div (aref Tab ip i) piv)))
          (dotimes (i m)
            (if (not (eq i ip))
                (let ((tm (aref Tab i jp)))
                  (if (not (meqp tm 0))
                      (dotimes (j n)
                        (setf (aref Tab i j)
                              (sub (aref Tab i j)
                                   (mul tm (aref Tab ip j)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Run artificial variable out of basis.                                     ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-out-of-basis-sx (Tab m n basis i)
  (let ((jp nil))
    (do ((j 0 (1+ j)))
        ((or (= j n) (not (null jp))))
      (if (not (meqp 0 (aref Tab i j))) ; if Tab[i,j]#0 then column j is not
          (setq jp j)))                 ; in the basis
    (if (null jp)
        nil
        (progn
          (setf (nth i basis) jp)
          (pivot-sx Tab i jp (+ 2 m) (+ 1 n))
          1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Scaling for the simplex algorithm. (Equilibratium scaling)                ;;
;;                                                                           ;;
;; After scaling, the maximum absolute value in each row/column is 1.        ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scale-sx (Tab m n sc-fac)
  (let ((r 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Scale the rows of A and b.                                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (dotimes (i (- m 2))
      (setq r 0)
      (dotimes (j n)
        (let* ((tij (aref Tab i j))
               (ta (if (mlsp tij 0) (neg tij) tij)))
          (if (mlsp r ta)
              (setq r ta))))
      (if (mlsp $epsilon_lp r)
          (dotimes (j n)
            (setf (aref Tab i j) (div (aref Tab i j) r)))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Scale the columns of A and c.                                ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (dotimes (j (1- n))
      (setq r 0)
      (dotimes (i (- m 2))
        (let* ((tij (aref Tab i j))
               (ta (if (mlsp tij 0) (neg tij) tij)))
          (if (mlsp r ta)
              (setq r ta))))
      (if (mlsp $epsilon_lp r)
          (progn
            (dotimes (i m)
              (setf (aref Tab i j) (div (aref Tab i j) r)))
            (setf (nth j sc-fac) r))))))
