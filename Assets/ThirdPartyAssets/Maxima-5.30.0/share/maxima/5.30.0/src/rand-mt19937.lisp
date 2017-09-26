;;; Mersenne Twister MT19937, adapted from CMUCL rand-mt19937.lisp -r1.11 (2003/03/06)

;;; CMUCL version by Douglas T. Crosher and Raymond Toy based
;;; on public domain code from Carnegie Mellon University.
;;; Modified for Maxima by Robert Dodier.
;;; (1) Construct floating point numbers using portable operations.
;;; (2) Construct large integers using all bits of each chunk.

;;; Begin MT19937 implementation.
;;; **********************************************************************
;;;
;;; Support for the Mersenne Twister, MT19937, random number generator
;;; due to Matsumoto and Nishimura. This implementation has been
;;; placed in the public domain with permission from M. Matsumoto.
;;;
;;; Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;; 623-dimensionally equidistributed uniform pseudorandom number
;;; generator.", ACM Transactions on Modeling and Computer Simulation,
;;; 1997, to appear.

(in-package :mt19937)

(defconstant mt19937-n 624)
(defconstant mt19937-m 397)
(defconstant mt19937-upper-mask #x80000000)
(defconstant mt19937-lower-mask #x7fffffff)
(defconstant mt19937-b #x9D2C5680)
(defconstant mt19937-c #xEFC60000)
;;;
;;;; Random state hackery:

;;; The state is stored in a (simple-array (unsigned-byte 32) (627))
;;; wrapped in a random-state structure:
;;;
;;;  0-1:   Constant matrix A. [0, #x9908b0df]
;;;  2:     Index k.
;;;  3-626: State.

;; GENERATE-SEED
;;
;; Generate a random seed that can be used for seeding the generator.
;; The current time is used as the seed.

(defun generate-seed ()
         (logand (get-universal-time) #xffffffff))

;; New initializer proposed by Takuji Nishimura and Makota Matsumoto.
;; (See http://www.math.keio.ac.jp/~matumoto/MT2002/emt19937ar.html)
;;
;; This corrects a deficiency in the original initializer wherein the
;; MSB of the seed was not well represented in the state.
;;
;; The initialization routine is described below.  Let s be the seed,
;; mt[] be the state vector.  Then the algorithm is
;;
;; mt[0] = s & 0xffffffffUL
;;
;; for (k = 1; k < N; k++) {
;;   mt[k] = 1812433253 * (mt[k-1] ^ (mt[k-1] >> 30)) + k
;;   mt[k] &= 0xffffffffUL
;; }
;;
;; The multiplier is from Knuth TAOCP Vol2, 3rd Ed., p. 106.
;;

(defun int-init-random-state (&optional (seed 5489) state)
  (declare (type (integer 0 #xffffffff) seed))
  (let ((state (or state (make-array 627 :element-type '(unsigned-byte 32)))))
    (declare (type (simple-array (unsigned-byte 32) (627)) state))
    (setf (aref state 0) 0)
    (setf (aref state 1) #x9908b0df)
    (setf (aref state 2) mt19937-n)
    (setf (aref state 3) seed)
    (do ((k 1 (1+ k)))
	((>= k 624))
      (declare (type (mod 625) k))
      (let ((prev (aref state (+ 3 (1- k)))))
	(setf (aref state (+ 3 k))
	      (logand (+ (* 1812433253 (logxor prev (ash prev -30)))
			 k)
		      #xffffffff))))
    state))

;; Initialize from an array.
;;
;; Here is the algorithm, in C.  init_genrand is the initalizer above,
;; init_key is the seed vector of length key_length.
;;
;;     init_genrand(19650218UL);
;;     i=1; j=0;
;;     k = (N>key_length ? N : key_length);
;;     for (; k; k--) {
;;         mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
;;           + init_key[j] + j; /* non linear */
;;         mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
;;         i++; j++;
;;         if (i>=N) {
;;           mt[0] = mt[N-1]; i=1;
;;         }
;;         if (j>=key_length) {
;;           j=0;
;;         }
;;     }
;;     for (k=N-1; k; k--) {
;;         mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
;;           - i; /* non linear */
;;         mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
;;         i++;
;;         if (i>=N) { mt[0] = mt[N-1]; i=1; }
;;     }
;;
;;     mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
;;

(defun vec-init-random-state (key &optional state)
  (declare (type (array (unsigned-byte 32) (*)) key))
  (let ((key-len (length key))
	(state (init-random-state 19650218 state))
	(i 1)
	(j 0))
    (loop for k from (max key-len mt19937-n) above 0 do
	  (let ((prev (aref state (+ 3 (1- i)))))
	    (setf (aref state (+ 3 i))
		  (ldb (byte 32 0)
		       (+ (aref key j) j
			  (logxor (aref state (+ 3 i))
				  (ldb (byte 32 0)
				       (* 1664525
					  (logxor prev (ash prev -30))))))))
	    (incf i)
	    (incf j)
	    (when (>= i mt19937-n)
	      (setf (aref state 3)
		    (aref state (+ 3 (- mt19937-n 1))))
	      (setf i 1))
	    (when (>= j key-len)
	      (setf j 0))))

    (loop for k from (1- mt19937-n) above 0 do
	  (let ((prev (aref state (+ 3 (1- i)))))
	    (setf (aref state (+ 3 i))
		  (ldb (byte 32 0)
		       (- (logxor (aref state (+ 3 i))
				  (* 1566083941
				     (logxor prev (ash prev -30))))
			  i)))
	    (incf i)
	    (when (>= i mt19937-n)
	      (setf (aref state 3)
		    (aref state (+ 3 (- mt19937-n 1))))
	      (setf i 1))))
    (setf (aref state 3) #x80000000)
    state))

;; 
(defun init-random-state (&optional (seed 5489) state)
  "Generate an random state vector from the given SEED.  The seed can be
  either an integer or a vector of (unsigned-byte 32)"
  (declare (type (or null integer
		     (array (unsigned-byte 32) (*)))
		 seed))
  (etypecase seed
    (integer
     (int-init-random-state (ldb (byte 32 0) seed) state))
    ((array (unsigned-byte 32) (*))
     (vec-init-random-state seed state))))

(defstruct (random-state
	     (:constructor make-random-object))
  (state (init-random-state) :type (simple-array (unsigned-byte 32) (627))))

(defvar *random-state* (make-random-object))

(defun make-random-state (&optional state)
  "Make a random state object.  If STATE is not supplied, return a copy
  of the default random state.  If STATE is a random state, then return a
  copy of STATE.  If STATE is T then return a random state generated from
  the universal time.  To make a random state from an integer seed, try
  ``(make-random-object :state (init-random-state <seed>))''."
  (flet ((copy-random-state (state)
	   (let ((state (random-state-state state))
		 (new-state
		  (make-array 627 :element-type '(unsigned-byte 32))))
	     (dotimes (i 627)
	       (setf (aref new-state i) (aref state i)))
	     (make-random-object :state new-state))))
    (cond ((not state) (copy-random-state *random-state*))
	  ((random-state-p state) (copy-random-state state))
	  ((eq state t)
	   (make-random-object :state (init-random-state (generate-seed))))
	  (t (error (intl:gettext "make_random_state: argument must be a random state object, or true, or false; found: ~S") state)))))

;;;; Random entries:

;;; Size of the chunks returned by random-chunk.
;;;
(defconstant random-chunk-length 32)

;;; random-chunk -- Internal
;;;
;;; This function generaters a 32bit integer between 0 and #xffffffff
;;; inclusive.
;;;
(declaim (inline random-chunk))
;;;
;;; Portable implementation.
(defun random-mt19937-update (state)
  (declare (type (simple-array (unsigned-byte 32) (627)) state)
	   (optimize (speed 3) (safety 0)))
  (let ((y 0))
    (declare (type (unsigned-byte 32) y))
    (do ((kk 3 (1+ kk)))
	((>= kk (+ 3 (- mt19937-n mt19937-m))))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) mt19937-upper-mask)
		      (logand (aref state (1+ kk)) mt19937-lower-mask)))
      (setf (aref state kk) (logxor (aref state (+ kk mt19937-m))
				    (ash y -1) (aref state (logand y 1)))))
    (do ((kk (+ (- mt19937-n mt19937-m) 3) (1+ kk)))
	((>= kk (+ (1- mt19937-n) 3)))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) mt19937-upper-mask)
		      (logand (aref state (1+ kk)) mt19937-lower-mask)))
      (setf (aref state kk) (logxor (aref state (+ kk (- mt19937-m mt19937-n)))
				    (ash y -1) (aref state (logand y 1)))))
    (setf y (logior (logand (aref state (+ 3 (1- mt19937-n)))
			    mt19937-upper-mask)
		    (logand (aref state 3) mt19937-lower-mask)))
    (setf (aref state (+ 3 (1- mt19937-n)))
	  (logxor (aref state (+ 3 (1- mt19937-m)))
		  (ash y -1) (aref state (logand y 1)))))
  (values))
;;;
(defun random-chunk (state)
  (declare (type random-state state)
	   (optimize (speed 3) (safety 0)))
  (let* ((state (random-state-state state))
	 (k (aref state 2)))
    (declare (type (mod 628) k))
    (when (= k mt19937-n)
      (random-mt19937-update state)
      (setf k 0))
    (setf (aref state 2) (1+ k))
    (let ((y (aref state (+ 3 k))))
      (declare (type (unsigned-byte 32) y))
      (setf y (logxor y (ash y -11)))
      (setf y (logxor y (ash (logand y (ash mt19937-b -7)) 7)))
      (setf y (logxor y (ash (logand y (ash mt19937-c -15)) 15)))
      (setf y (logxor y (ash y -18)))
      y)))

;;; %RANDOM-SINGLE-FLOAT, %RANDOM-DOUBLE-FLOAT  --  Interface
;;;
(declaim (inline %random-single-float %random-double-float
		 #+(or scl clisp) %random-long-float
		 #+(and cmu double-double) %random-double-double-float))
;;;
(declaim (ftype (function ((single-float (0f0)) random-state)
			  (single-float 0f0))
		%random-single-float))
;;;
(declaim (ftype (function ((double-float (0d0)) random-state)
			  (double-float 0d0))
		%random-double-float))
;;;
#+(or scl clisp)
(declaim (ftype (function ((long-float (0l0)) random-state)
			  (long-float 0l0))
		%random-long-float))
;;;
#+(and cmu double-double)
(declaim (ftype (function ((kernel:double-double-float (0w0)) random-state)
			  (kernel:double-double-float 0w0))
		%random-double-double-float))
;;;
;;;
(defun %random-single-float (arg state)
  "Handle the single or double float case of RANDOM.  We generate a float
  in [0f0, 1f0) by clobbering the mantissa of 1f0 with random bits (23 bits);
  this yields a number in [1f0, 2f0). Then 1f0 is subtracted."
  (let*
    ((random-mantissa-bits (%random-integer (expt 2 23) state))
    (random-unit-float (- (scale-float (float (+ (expt 2 23) random-mantissa-bits) 1f0) -23) 1f0)))
  (* arg random-unit-float)))

(defun %random-double-float (arg state)
  "Handle the single or double float case of RANDOM.  We generate a float
  in [0d0, 1d0) by clobbering the mantissa of 1d0 with random bits (52 bits);
  this yields a number in [1d0, 2d0). Then 1d0 is subtracted."
  (let*
    ((random-mantissa-bits (%random-integer (expt 2 52) state))
    (random-unit-double (- (scale-float (float (+ (expt 2 52) random-mantissa-bits) 1d0) -52) 1d0)))
  (* arg random-unit-double)))

#+(or scl clisp)
(defun %random-long-float (arg state)
  "Handle the long float case of RANDOM.  We generate a float in [0l0, 1l0) by
  clobbering the mantissa of 1l0 with random bits; this yields a number in
  [1l0, 2l0). Then 1l0 is subtracted."
  (let* ((d (1- (float-digits 1l0)))
	 (m (expt 2 d))
	 (random-mantissa-bits (%random-integer m state))
	 (random-unit-double (- (scale-float (float (+ m random-mantissa-bits) 1l0) (- d)) 1l0)))
    (* arg random-unit-double)))

#+(and cmu double-double)
(defun %random-double-double-float (arg state)
  "Handle the double-double float case of RANDOM.  We generate a float in [0w0, 1w0) by
  clobbering the mantissa of 1w0 with random bits; this yields a number in
  [1w0, 2w0). Then 1w0 is subtracted."
  (let* ((d (1- (float-digits 1w0)))
	 (m (expt 2 d))
	 (random-mantissa-bits (%random-integer m state))
	 (random-unit-double (- (scale-float (float (+ m random-mantissa-bits) 1w0) (- d)) 1w0)))
    (* arg random-unit-double)))

;;;; Random integers:

;;; %RANDOM-INTEGER  --  Internal
;;;
(defun %random-integer (arg state)
  "Generates an integer greater than or equal to zero and less than Arg.
  Successive chunks are concatenated without overlap to construct integers
  larger than a single chunk. The return value has this property:
  If two integers are generated from the same state with Arg equal to 2^m and 2^n,
  respectively, then bit k is the same in both integers for 0 <= k < min(m,n).
  Each call to %RANDOM-INTEGER consumes at least one chunk; bits left over
  from previous chunks are not re-used."
  (declare (type (integer 1) arg) (type random-state state))
    (do*
      ((nchunks (ceiling (integer-length (1- arg)) random-chunk-length) (1- nchunks))
        (new-bits 0 (random-chunk state))
        (bits 0 (logior bits (ash new-bits shift)))
        (shift 0 (+ shift random-chunk-length)))
      ((= 0 nchunks)
        (rem bits arg))))

(defun random (arg &optional (state *random-state*))
  "Generates a uniformly distributed pseudo-random number greater than or equal to zero
  and less than Arg.  State, if supplied, is the random state to use."
  (declare (inline %random-single-float %random-double-float))
  (cond
    #-gcl  ; GCL's single and double floats are the same; route all floats through %random-double-float
    ((and (typep arg 'single-float) (> arg 0.0F0))
     (%random-single-float arg state))
    ((and (typep arg 'double-float) (> arg 0.0D0))
     (%random-double-float arg state))
    #+(or scl clisp)
    ((and (typep arg 'long-float) (> arg 0.0L0))
     (%random-long-float arg state))
    #+(and cmu double-double)
    ((and (typep arg 'kernel:double-double-float) (> arg 0.0W0))
     (%random-double-double-float arg state))
    ((and (integerp arg) (> arg 0))
     (%random-integer arg state))
    (t
     (error 'simple-type-error
	    :expected-type '(or (integer 1) (float (0))) :datum arg
	    :format-control (intl:gettext "random: argument must be a positive integer or a positive float; found: ~S")
	    :format-arguments (list arg)))))

;;; begin Maxima-specific stuff

(in-package :maxima)

(defmfun $set_random_state (x)
  "Copy the argument, and assign the copy to MT19937::*RANDOM-STATE*.
  Returns '$done."
  (setq mt19937::*random-state* (mt19937::make-random-state x))
  '$done)

(defmfun $make_random_state (x)
  "Returns a new random state object. If argument is an integer or array,
  use argument to initialize random state. Otherwise punt to MT19937::MAKE-RANDOM-STATE."
  (cond
    ((or (integerp x) (arrayp x))
      (mt19937::make-random-object :state (mt19937::init-random-state x)))
    (t
      (mt19937::make-random-state x))))

(defmfun $random (x)
  "Returns the next number from this generator.
  Punt to MT19937::RANDOM."
  (if (and (or (integerp x) (floatp x))
	   (> x 0))
      (mt19937::random x)
    (merror (intl:gettext "random: argument must be a positive integer or positive float; found: ~M") x)))

;;; end Maxima-specific stuff

