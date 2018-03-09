;;                 COPYRIGHT NOTICE
;;  
;;  Copyright (C) 2006 Mario Rodriguez Riotorto
;;  
;;  This program is free software; you can redistribute
;;  it and/or modify it under the terms of the
;;  GNU General Public License as published by
;;  the Free Software Foundation; either version 2 
;;  of the License, or (at your option) any later version. 
;;  
;;  This program is distributed in the hope that it
;;  will be useful, but WITHOUT ANY WARRANTY;
;;  without even the implied warranty of MERCHANTABILITY
;;  or FITNESS FOR A PARTICULAR PURPOSE. See the 
;;  GNU General Public License for more details at
;;  http://www.gnu.org/copyleft/gpl.html

;;  This is a set of numerical routines used by package 'stats'

;; For questions, suggestions, bugs and the like, feel free
;; to contact me at
;; mario @@@ edu DOT xunta DOT es
;; www.biomates.net


(in-package :maxima)

;; para traducir funciones de usuario a lisp:
;; (meval '($translate $random_normal))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       ;;
;;    Wilcoxon-Mann-Whitney recursion    ;;
;;                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  This recursion is used when computing exact probabilities
;;  for the distribution of the rank statistic in the Wilcoxon-
;;  Mann-Whitney test, specially for small samples. It's defined as
;;             a(i,j,k) = a(i,j-1,j-k) + a(i-k,j-1,j-k-1)
;;  Reference:
;;     Klotz, Jerome (2006) 'A Computational Approach to Statistics',
;;          available at www.stat.wisc.edu/~klotz/Book.pdf
(defun rank_sum_recursion (i j k)
    (cond ((or (< i 0) (< j 0) (< k 0)) 0)
          ((= i 0) 1)
          (t (+ (rank_sum_recursion i (- j 1) (- j k))
                (rank_sum_recursion (- i k) (- j 1) (+ j (- k) (- 1)) )))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       ;;
;;        Signed rank recursion          ;;
;;                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  This recursion is used when computing exact probabilities
;;  for the true distribution of statistic in the signed rank test,
;;  specially for small samples. It's defined as
;;             a(i,j) = a(i,j-1) + a(i-j,j-1)
;;  Reference:
;;     Klotz, Jerome (2006) 'A Computational Approach to Statistics',
;;          available at www.stat.wisc.edu/~klotz/Book.pdf
;;     R statistical package. File signrank.c
(defun signed_rank_recursion (i j)
  (let* ((u (/ (* j (+ j 1)) 2))
         (c (floor (/ u 4))))
    (if (> i c) (setf i (- u i)))
    (cond ((or (< i 0) (< j 0)) 0)
          ((= i 0) 1)
          (t (+ (signed_rank_recursion i (- j 1))
                (signed_rank_recursion (- i j) (- j 1)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       ;;
;;    Shapiro-Wilk test for normality    ;;
;;                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Calculates the algebraic polynomial of order (- (lengh cc) 1)
;; with array of coefficients cc. Zero order coefficient is (aref cc 0).
(defun swpoly (cc x)
  (let ( (ret-val (aref cc 0))
         (nord (length cc))
         p)
    (when (> nord 1) 
      (setf p (* x (aref cc (1- nord))))
      (do ((j (- nord 2) (1- j)))
          ((= j 0) 'done)
        (setf p (* x (+ p (aref cc j)))))
      (setf ret-val (+ ret-val p)) )
    ret-val ) )


;;  Calculates the Shapiro-Wilk test and its significance level
;;  This is a translation from Fortran to Lisp of swilk.f, which translation
;;  to C is used by the R statistical package. This is
;;  Algorithm AS R94, Applied Statistics (1995), vol.44, no.4, 547-551
;;  Argument 'x' is a Maxima list containing numbers. Optional argument 'n1'
;;  is the number of uncensored observations; this option is not documented
;;  in Maxima at this moment, since I haven't checked it; in fact, the R 
;;  function 'shapiro.test' doesn't make use of this second argument (I don't know why).
(defun $test_normality (x &optional n1)
  (declare (special $stats_numer))
   (unless (or ($listp x)
	       (and ($matrixp x) (= ($length ($first x)) 1)))
     (merror "First argument of 'test_normality' must be a Maxima list"))
   (when ($matrixp x)
     (setq x ($first ($transpose x))))
   (setf x (sort (map 'list #'$float (rest x)) #'<))
   (if (null n1) (setf n1 (length x)))
   (let* (($numer $stats_numer)
          (n (length x))
          (n2 (floor (/ n 2)))
          w pw  ; W statistic and its p-value
          ; initialized data
          (z90 1.2816)  (z95 1.6449)
          (z99 2.3263)  (zm 1.7509)   (zss 0.56268)
          (bf1 0.8378)  (xx90 0.556)  (xx95 0.622)
          (sqrth 0.70711)  (small 1e-19)  (pi6 1.909859) (stqr 1.047198)
          ; polynomial coefficients
          (g (make-array 2 :element-type 'flonum
               :initial-contents '(-2.273 0.459)))
          (c1 (make-array 6 :element-type 'flonum
               :initial-contents '(0.0 0.221157 -0.147981 -2.07119 4.434685 -2.706056)))
          (c2 (make-array 6 :element-type 'flonum
               :initial-contents '(0.0 0.042981 -0.293762 -1.752461 5.682633 -3.582633)))
          (c3 (make-array 4 :element-type 'flonum
               :initial-contents '(0.544 -0.39978 0.025054 -6.714e-4)))
          (c4 (make-array 4 :element-type 'flonum
               :initial-contents '(1.3822 -0.77857 0.062767 -0.0020322)))
          (c5 (make-array 4 :element-type 'flonum
               :initial-contents '(-1.5861 -0.31082 -0.083751 0.0038915)))
          (c6 (make-array 3 :element-type 'flonum
               :initial-contents '(-0.4803 -0.082676 0.0030302)))
          (c7 (make-array 2 :element-type 'flonum
               :initial-contents '(0.164 0.533)))
          (c8 (make-array 2 :element-type 'flonum
               :initial-contents '(0.1736 0.315)))
          (c9 (make-array 2 :element-type 'flonum
               :initial-contents '(0.256 -0.00635)))
          ncens nn2 i1 range delta a2 a1 an25 an rsn fac ssumm2 summ2 xi xx y w1 
          ssassx xsx asa sax ssa ssx sx sa zbar zsd zfm z99f z95f 
          z90f bf ld s m gamma a)

      (setf an (coerce n 'flonum))
      (setf ncens (- n n1))   ; number of censored observations
      (setf nn2 (truncate n 2))
      (if (or (< n 3) (> n 5000))
         (merror "Sample size must be between 3 and 5000"))
      (setf range (- (car (last x)) (first x)))
      (if (< range small)
         (merror "All observations are identical"))
      (if (< n1 3) (merror "Number of uncensored observations is too small"))
      (if (< ncens 0) 
         (merror "Number of uncensored observations is greater than total sample size!!"))
      (if (and (> ncens 0) (< n 20))
         (merror "Sample size is too small, and censored data are not allowed"))
      (setf delta (/ ncens an))
      (if (> delta 0.8)
         (merror "Ratio of censored observations is too great (>80%)"))

      ; calculate coefficients for statistic w
      (setf a (make-array (1+ n2) :element-type 'flonum :initial-element 0.0))
      (cond ((= n 3)
                (setf (aref a 1) sqrth))
            (t  (setf an25 (+ 0.25 an))
                (setf summ2 0.0)
                (do ((i 1 (1+ i)))
                   ((> i n2) 'done)
                   (setf (aref a i)
                         (coerce (mul* 1.414213562373095
                                       (simplify
                                          (list '(%inverse_erf)
                                                (add* (mul* 2.0 (/ (- i 0.375) an25)) -1))))
                                 'flonum))
                   (setf summ2 (+ summ2 (* (aref a i) (aref a i)))))
                (setf summ2 (* summ2 2.0))
                (setf ssumm2 (sqrt summ2))
                (setf rsn (/ 1.0 (sqrt an)))
                (setf a1 (- (swpoly c1 rsn) (/ (aref a 1) ssumm2)))
                ; normalize a
                (cond ((> n 5)
                         (setf i1 3)
                         (setf a2 (- (swpoly c2 rsn) (/ (aref a 2) ssumm2) ))
                         (setf fac (sqrt (/ (+ summ2
                                               (* (aref a 1) (aref a 1) -2.0)
                                               (* (aref a 2) (aref a 2) -2.0))
                                            (+ 1.0 (* a1 a1 -2.0) (* a2 a2 -2.0)))))
                         (setf (aref a 2) a2)  )
                      (t (setf i1 2)
                         (setf fac (sqrt (/ (- summ2 (* 2.0 (aref a 1) (aref a 1)))
                                            (- 1.0 (* 2.0 a1 a1)))))     ))
                (setf (aref a 1) a1)
                (do ((i i1 (1+ i)))
                     ((> i nn2) 'done)
                   (setf (aref a i) (/ (aref a i) (- fac))))     ))

      ; check for correct sort order on range - scaled X
      (setf xx (/ (first x) range))
      (setf sx xx)
      (setf sa (- (aref a 1)))
      (do ((j (1- n) (1- j))
           (i 1 i))
          ((>= i n1) 'done)
        (setf xi (/ (nth i x) range))
        (setf sx (+ sx xi))
        (setf i (1+ i))
        (if (/= i j)
            (setf sa (+ sa (* (signum (- i j)) (aref a (min i j))))))
        (setf xx xi) )

      ; Calculate W statistic as squared correlation
      ; between data and coefficients
      (setf sa (/ sa n1))
      (setf sx (/ sx n1))
      (setf ssa 0.0
            ssx 0.0
            sax 0.0)
      (do ((j (1- n) (1- j))
           (i 0 (1+ i)))
        ((>= i n1) 'done)
        (if (/= i j)
            (setf asa (- (* (signum (- i j)) (aref a (+ 1 (min i j)))) sa))
            (setf asa (- sa)))
        (setf xsx (- (/ (nth i x) range) sx))
        (setf ssa (+ ssa (* asa asa)))
        (setf ssx (+ ssx (* xsx xsx)))
        (setf sax (+ sax (* asa xsx)))  )

      ; w1 equals (1-w) calculated to avoid excessive rounding error
      ; for W very near 1 (a potential problem in very large samples)
      (setf ssassx (sqrt (* ssa ssx)))
      (setf w1 (/ (* (- ssassx sax) (+ ssassx sax))
                  (* ssa ssx)))
      (setf w (- 1 w1))

      ; calculate significance level for w
      (tagbody
         (when (= n 3)
            (setf pw (* pi6 (- ($asin (sqrt w)) stqr)))
            (go fin))
         (setf y  (log w1)
               xx (log an))
         (cond ((<= n 11)
                  (setf gamma (swpoly g an))
                  (when (>= y gamma)
                     (setf pw small)
                     (go fin))
                  (setf y (- (log (- gamma y)))
                        m (swpoly c3 an)
                        s (exp (swpoly c4 an))))
               (t ; n >= 12
                  (setf m (swpoly c5 xx)
                        s (exp (swpoly c6 xx)))))

          ; censoring by proportion ncens/n
          ; calculate mean and sd of normal equivalent deviate of w
          (when (> ncens 0)
             (setf ld (- (log delta))
                   bf (+ 1.0 (* xx bf1)))
             (setf z90f (+ z90 (* bf (expt (swpoly c7 (expt xx90 xx)) ld))))
             (setf z95f (+ z95 (* bf (expt (swpoly c8 (expt xx95 xx)) ld))))
             (setf z99f (+ z99 (* bf (expt (swpoly c9 xx) ld))))
             ; regress z90F,...,z99F on normal deviates z90,...,z99 to get
             ; pseudo-mean and pseudo-sd of z as the slope and intercept
             (setf zfm (/ (+ z90f z95f z99f) 3.0))
             (setf zsd (/ (+ (* z90 (- z90f zfm))
                             (* z95 (- z95f zfm))
                             (* z99 (- z99f zfm)))
                          zss))
             (setf zbar (- zfm (* zsd zm)))
             (setf m (+ m (* zbar s)))
             (setf s (* s zsd)) )

          (setf pw (- 0.5 (* 0.5 (mfuncall '%erf (/ (- y m)
                                                    (* 1.41421356 s))))))
        fin)

      ; the result is an 'inference_result' Maxima object
      ($inference_result
         "SHAPIRO - WILK TEST"
         (list '(mlist) (list '(mlist) '$statistic w)
                        (list '(mlist) '$p_value pw))
         (list '(mlist) 1 2) )  ))



