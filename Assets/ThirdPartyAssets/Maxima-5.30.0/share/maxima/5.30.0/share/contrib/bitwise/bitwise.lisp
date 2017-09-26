
;; Maxima bit functions
;; Copyright (C) 2008 Volker van Nek

;; This source code is licensed under the terms of the Lisp Lesser 
;; GNU Public License (LLGPL). The LLGPL consists of a preamble, published
;; by Franz Inc. (http://opensource.franz.com/preamble.html), and the GNU 
;; Library General Public License (LGPL), version 2, or (at your option)
;; any later version.  When the preamble conflicts with the LGPL, 
;; the preamble takes precedence. 

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Library General Public License for details.

;; You should have received a copy of the GNU Library General Public
;; License along with this library; if not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301, USA.


;; functions at Maxima level:

;; bit_not      bitwise NOT
;; bit_and      bitwise AND
;; bit_xor      bitwise XOR
;; bit_or       bitwise OR
;; bit_lsh      bitwise LEFT SHIFT
;; bit_rsh      bitwise RIGHT SHIFT

;; bit_length   number of necessary bits to represent a positive integer
;; bit_onep     test for bit 1


;; comments in this file use the following abbreviations:
;; i,j,k : literal integers
;; x,y,z : any expression
;; di,dj,dk : declared integer
;; de, do : declared even resp. odd

(in-package :maxima)

(macsyma-module bitwise)



;; I'm not sure if we should add the property 'integer-valued to these functions
;; if we allow bit_not(bit_not(x)) --> x, bit_and(x) --> x, etc. for any expression x
(setf (get '$bit_not 'integer-valued) t)
(setf (get '$bit_and 'integer-valued) t)
(setf (get '$bit_xor 'integer-valued) t)
(setf (get '$bit_or 'integer-valued) t)
(setf (get '$bit_lsh 'integer-valued) t)
(setf (get '$bit_rsh 'integer-valued) t)
(setf (get '$bit_length 'integer-valued) t)



;; ERROR if at least one arg to bit_function is a non-integer constant, string or Maxima list
;;
(defun badargcheck (a f n) 
  (if (or (and (not (integerp a)) ($constantp a)) 
          (stringp a) 
          ($listp a))
    (let ((s (format nil "bad ~@[~:r~] argument to `~a': ~a" n f ($sconcat a))))
      (merror "~m" s))))



;; bitwise NOT
;;
(defprop $bit_not simp-bit-not operators)
;;
(defun simp-bit-not (a tmp z)
  (declare (ignore tmp))
  (oneargcheck a)
  (setq a (simplifya (cadr a) z))
  (badargcheck a "bit_not" nil)
  (cond ((integerp a)                              ;; bit_not(i) --> bitwise NOT i
          (lognot a))                    
        ((and (listp a) (listp (car a)) (equal (caar a) '$bit_not))
          (cadr a))                                ;; bit_not(bit_not(x)) --> x ;; di instead of x ?
        ((maxima-integerp a)                       ;; bit_not(di) -->  - di - 1
          (meval `((mplus) ((mminus) ,a) -1)))     
        (t
         `(($bit_not simp) ,a)) ))                 ;; return unevaluated



;; bitwise AND
;;
(defprop $bit_and simp-bit-and operators)
;;
(defun simp-bit-and (args tmp z)
  (declare (ignore tmp))
  (setq args (cdr args))

  ;; trivial case                                     bit_and() --> -1
  (if (null args) (return-from simp-bit-and -1))
  
  (let ((acc nil) (n 1) (ints nil)) 

  ;; check and simplify the arguments
    (dolist (a args) 
      (badargcheck a "bit_and" n) 
      (setq n (1+ n))
      (push (simplifya a z) acc) )
    
  ;; separate the arguments into a list of literal integers and the rest
  ;; and remove duplicates                            bit_and(x,x,y) --> bit_and(x,y)
    (setq args (sorted-remove-duplicates (sort acc '$orderlessp)))
    (do ((a (car args)))
        ((or (null args) (not (integerp a))))
      (push a ints)
      (setq args (cdr args))
      (setq a (car args)) )
    
  ;; apply logand to the list of literal integers and return if possible
    (setq n (apply #'logand ints))
    (if (null args) (return-from simp-bit-and n))  ;; bit_and(i,j) --> bitwise i AND j
    (cond ((zerop n) (return-from simp-bit-and 0)) ;; bit_and(0,x) --> 0
          ((= n -1) (setq ints nil))               ;; bit_and(-1,x) --> bit_and(x) ( --> x  see below )
          (t (setq ints (list n))))                ;; bit_and(i,j,x) --> bit_and(bit_and(i,j),x)
        
  ;; if twos complement occurs                        bit_and(x,bit_not(x),y) --> 0
    (setq acc nil)
    (do ((a (car args)))
        ((null (cdr args)) (push a acc))
      (if (some #'(lambda (b) (equal t (meval `(($is) (($equal) (($bit_not simp) ,a) ,b))))) (cdr args))
        (return-from simp-bit-and 0))
      (push a acc)
      (setq args (cdr args))
      (setq a (car args)) )
    
  ;; even or odd declared objects                     bit_and(1,de,y) --> 0
    (if (and (= n 1) (some #'(lambda (b) ($featurep b '$even)) acc))
      (return-from simp-bit-and 0))
  ;;                                                  bit_and(1,do) --> 1
    (if (and (= n 1) (every #'(lambda (b) ($featurep b '$odd)) acc))
      (return-from simp-bit-and 1))

  ;; if one arg remains                               bit_and(x) --> x ;; di instead of x ?
    (setq args (append ints (reverse acc)))
    (if (= 1 (length args))
      ;;(and (= 1 (length args)) (maxima-integerp (car args))); if we require bit_and(di) --> di
      (return-from simp-bit-and (car args)))
    
  ;; return unevaluated if no previous return-from occured
   `(($bit_and simp) ,@args)))



;; helpers for $bit_xor
;;
(defun sorted-remove-pairs (l)
  (do ((a (car l) (car l)) (acc nil))
      ((null l) (reverse acc))
    (cond ((and (cdr l) (like a (cadr l)))
		       (setq l (cddr l)))
          (t (push a acc)
             (setq l (cdr l))) )))
;;
(defun remove-twos-complement (n l)
  (do ((a (car l) (car l)) (acc nil))
      ((null l) (reverse acc))
    (cond ((equal t (meval `(($is) (($equal) (($bit_not) ,a) ,n))))
             (return-from remove-twos-complement (append (reverse acc) (cdr l))))
          (t (push a acc)
             (setq l (cdr l))) )))
  


;; bitwise EXCLUSIVE OR
;;
(defprop $bit_xor simp-bit-xor operators)
;;
(defun simp-bit-xor (args tmp z)
  (declare (ignore tmp))
  (setq args (cdr args))

  ;; trivial case                                     bit_xor() --> 0
  (if (null args) (return-from simp-bit-xor 0))
  
  (let ((acc nil) (n 1) (ints nil)) 

  ;; check and simplify the arguments
    (dolist (a args) 
      (badargcheck a "bit_xor" n) 
      (setq n (1+ n))
      (push (simplifya a z) acc) )
    
  ;; separate the arguments into a list of literal integers and the rest
  ;; and remove pairs                                 bit_xor(x,x,y,z) --> bit_xor(y,z)
    
    (setq args (sorted-remove-pairs (sort acc '$orderlessp)))
    (do ((a (car args)))
        ((or (null args) (not (integerp a))))
      (push a ints)
      (setq args (cdr args))
      (setq a (car args)) )
    
  ;; apply logxor to the list of integers and return if possible
    (setq n (apply #'logxor ints))
    (if (null args) (return-from simp-bit-xor n))  ;; bit_xor(i,j) --> bitwise i XOR j
    (setq ints (list n))                           ;; bit_xor(i,j,x) --> bit_xor(bit_xor(i,j),x)
        
  ;; if twos complement occurs                        bit_xor(x,bit_not(x),y,z) --> bit_xor(-1,y,z)
    (setq acc nil)
    (do ((a (car args)))
        ((null (cdr args)) (and a (push a acc)))
      (cond ((some #'(lambda (b) (equal t (meval `(($is) (($equal) (($bit_not) ,a) ,b))))) (cdr args))
               (rplacd args (remove-twos-complement a (cdr args)))
               (setq ints (list (setq n (1- (- n))))))
            (t (push a acc)))
      (setq args (cdr args))
      (setq a (car args)) )
    (setq acc (reverse acc))

  ;; n might have been changed in previous step; 0 occurs
    (if (zerop n) (setq ints nil))                 ;; bit_xor(0,x) --> bit_xor(x) ( --> x  see below)
    
  ;; if there is an even or odd declared object       bit_xor(1,de,y) --> bit_xor(de+1,y) --> recurse
    (if (and (= n 1) (some #'(lambda (b) ($featurep b '$even)) acc))
      (progn
        (do ((a (car acc) (car acc)) (bcc nil))
            ((null acc) (setq acc (reverse bcc)))
          (if ($featurep a '$even) 
            (progn 
              (setq acc (append (reverse bcc) (list (meval `((mplus) ,a 1))) (cdr acc)))
              (setq n 0)
              (setq ints nil)
              (return-from simp-bit-xor (meval `(($bit_xor simp) ,@acc))) ))
          (push a bcc)
          (setq acc (cdr acc))) ))
  ;;                                                  bit_xor(1,do,y) --> bit_xor(do-1,y) --> recurse
    (if (and (= n 1) (some #'(lambda (b) ($featurep b '$odd)) acc))
      (progn
        (do ((a (car acc) (car acc)) (bcc nil))
            ((null acc) (setq acc (reverse bcc)))
          (if ($featurep a '$odd) 
            (progn 
              (setq acc (append (reverse bcc) (list (meval `((mplus) ,a ((mminus) 1)))) (cdr acc)))
              (setq n 0)
              (setq ints nil)
              (return-from simp-bit-xor (meval `(($bit_xor simp) ,@acc))) ))
          (push a bcc)
          (setq acc (cdr acc))) ))

  ;; -1 occurs                                        bit_xor(-1,x,y) --> bit_xor(bit_not(x),y) --> recurse
    (if (and acc (= n -1))
      (return-from simp-bit-xor 
        (meval `(($bit_xor simp) 
                  ,@(cons (meval `(($bit_not simp) ,(car acc))) (cdr acc)) )) ))

  ;; if one arg remains                               bit_xor(x) --> x ;; di instead of x ?
    (setq args (append ints acc))
    (if (= 1 (length args))
      (return-from simp-bit-xor (car args)))
    
  ;; return unevaluated if no previous return-from occured
   `(($bit_xor simp) ,@args)))



;; bitwise OR
;;
(defprop $bit_or simp-bit-or operators)
;;
(defun simp-bit-or (args tmp z)
  (declare (ignore tmp))
  (setq args (cdr args))

  ;; trivial case                                     bit_or() --> 0
  (if (null args) (return-from simp-bit-or 0))
  
  (let ((acc nil) (n 1) (ints nil)) 

  ;; check and simplify the arguments
    (dolist (a args) 
      (badargcheck a "bit_or" n) 
      (setq n (1+ n))
      (push (simplifya a z) acc) )
    
  ;; separate the arguments into a list of literal integers and the rest
  ;; and remove duplicates                            bit_or(x,x,y) --> bit_or(x,y)
    (setq args (sorted-remove-duplicates (sort acc '$orderlessp)))
    (do ((a (car args)))
        ((or (null args) (not (integerp a))))
      (push a ints)
      (setq args (cdr args))
      (setq a (car args)) )
    
  ;; apply logior to the list of literal integers and return if possible
    (setq n (apply #'logior ints))
    (if (null args) (return-from simp-bit-or n))   ;; bit_or(i,j) --> bitwise i OR j
    (cond ((= n -1) (return-from simp-bit-or -1))  ;; bit_or(-1,x) --> -1
          ((zerop n) (setq ints nil))              ;; bit_or(0,x) --> bit_or(x) ( --> x  see below)
          (t (setq ints (list n))))                ;; bit_or(i,j,x) --> bit_or(bit_or(i,j),x)
        
  ;; if twos complement occurs                        bit_or(x,bit_not(x),y) --> -1
    (setq acc nil)
    (do ((a (car args)))
        ((null (cdr args)) (push a acc))
      (if (some #'(lambda (b) (equal t (meval `(($is) (($equal) (($bit_not simp) ,a) ,b))))) (cdr args))
        (return-from simp-bit-or -1))
      (push a acc)
      (setq args (cdr args))
      (setq a (car args)) )
    
  ;; if there is an even or odd declared obj          bit_or(1,de,y) --> bit_or(de+1,y) --> recurse
    (if (and (= n 1) (some #'(lambda (b) ($featurep b '$even)) acc))
      (progn
        (do ((a (car acc) (car acc)) (bcc nil))
            ((null acc) (setq acc (reverse bcc)))
          (if ($featurep a '$even) 
            (progn 
              (setq acc (append (reverse bcc) (list (meval `((mplus) ,a 1))) (cdr acc)))
              (setq n 0)
              (setq ints nil)
              (return-from simp-bit-or (meval `(($bit_or simp) ,@acc))) ))
          (push a bcc)
          (setq acc (cdr acc))) ))
  ;;                                                  bit_or(1,do,y) --> bit_or(do,y)
    (if (and (= n 1) (some #'(lambda (b) ($featurep b '$odd)) acc))
      (setq ints nil))

  ;; if one arg remains                               bit_or(x) --> x ;; di instead of x ?
    (setq args (append ints (reverse acc)))
    (if (= 1 (length args))
      (return-from simp-bit-or (car args)))
    
  ;; return unevaluated if no previous return-from occured
   `(($bit_or simp) ,@args)))



;; bitwise LEFT SHIFT
;;
(defprop $bit_lsh simp-bit-lsh operators)

(defun simp-bit-lsh (e tmp z)
  (declare (ignore tmp))
  (twoargcheck e)
  (let ((a (simplifya (cadr e) z)) 
        (count (simplifya (caddr e) z)))
    (badargcheck a "bit_lsh" 1)
    (badargcheck count "bit_lsh" 2)
    (cond ((and (integerp a) (integerp count))          ;; bit_lsh(i,k) --> bitwise LEFT SHIFT i,k
            (ash a count))                  
          ((and (maxima-integerp count) (equal (meval `(($is) ((mgeqp) ,count 0))) t))            
            (meval `((mtimes) ((mexpt) 2 ,count) ,a)))  ;; bit_lsh(x,dk) --> 2^dk*x, where dk>=0 ;; di instead of x ?
          (t
           `(($bit_lsh simp) ,a ,count)) )))            ;; return unevaluated



;; bitwise RIGHT SHIFT
;;
(defprop $bit_rsh simp-bit-rsh operators)
;;
(defun simp-bit-rsh (e tmp z)
  (declare (ignore tmp))
  (twoargcheck e)
  (let ((a (simplifya (cadr e) z)) 
        (count (simplifya (caddr e) z)))
    (badargcheck a "bit_rsh" 1)
    (badargcheck count "bit_rsh" 2)
    (meval `(($bit_lsh) ,a (- ,count)))))               ;; bit_rsh(x,y) --> bit_lsh(x,-y)



;; ONE-BIT TEST
;;
(defprop $bit_onep simp-bit-onep operators)
;;
(defun simp-bit-onep (e tmp z)
  (declare (ignore tmp))
  (twoargcheck e)
  (let ((a (simplifya (cadr e) z)) 
        (index (simplifya (caddr e) z)))
    (badargcheck a "bit_onep" 1)
    (badargcheck index "bit_onep" 2)
    (if (equal t (meval `(($is) ((mlessp) ,index 0))))  ;; additional check: error if index<0
      (merror "second argument to `bit_onep' must be non-negative."))
    (cond ((and (integerp a) (integerp index))          ;; bit_onep(i,k) --> ONE-BIT TEST i,k, k>=0
            (logbitp index a))                  
          ((eql 0 index) 
            (cond (($featurep a '$even) nil)            ;; bit_onep(de,0) --> false
                  (($featurep a '$odd) t)))             ;; bit_onep(do,0) --> true
          ((and (maxima-integerp index)                 ;; bit_onep(x,dk) where 0<=x<2^dk, dk>=0 --> false
                (equal t (meval `(($is) ((mgeqp) ,index 0))))
                (equal t (meval `(($is) ((mgeqp) ,a 0)))) 
                (equal t (meval `(($is) ((mlessp) ,a ((mexpt) 2 ,index))))))
            nil)
          ((and                                         ;; bit_onep(di^dj,y) where di^dj = 2^y --> true
                (listp a) (listp (car a)) (equal (caar a) 'mexpt)
                ;; check if  y = dj*log(di)/log(2) :
                (maxima-integerp (cadr a))
                (maxima-integerp (caddr a))
                (equal t 
                       (meval `(($is) (($equal)
                         ,index
                         ((mquotient) ((mtimes) ,(caddr a) ((%log) (($factor) ,(cadr a)))) ((%log) 2)) )))))
            t)
          (t
           `(($bit_onep simp) ,a ,index)) )))           ;; return unevaluated



;; BIT LENGTH
;;
(defprop $bit_length simp-bit-length operators)
;;
(defun simp-bit-length (a tmp z)
  (declare (ignore tmp))
  (oneargcheck a)
  (setq a (simplifya (cadr a) z))
  (badargcheck a "bit_length" nil)
  (if (equal t (meval `(($is) ((mlessp) ,a 0))))      ;; additional check: error if a<0
    (merror "argument to `bit_length' must be non-negative."))
  (cond ((integerp a)                                 ;; bit_length(i) --> BIT LENGTH i
          (integer-length a))
        ((and                                         ;; bit_length(2^dk) --> dk+1, bit_length(4^dk) --> 2*dk+1, etc.
                                                      ;; where dk>=0
              (listp a) (listp (car a)) (equal (caar a) 'mexpt)
              (maxima-integerp (cadr a))
              (maxima-integerp (caddr a)) (equal t (meval `(($is) ((mgeqp) ,(caddr a) 0))))
              (let ((fl (get-factor-list (cadr a))) e)
                (and (= 1 (length fl)) (= 2 (caar fl)))
                     (setq e (cadar fl)) 
                     (return-from simp-bit-length (meval `((mplus) ((mtimes) ,e ,(caddr a)) 1)) ))))
        (t
         `(($bit_length simp) ,a)) ))                 ;; return unevaluated

