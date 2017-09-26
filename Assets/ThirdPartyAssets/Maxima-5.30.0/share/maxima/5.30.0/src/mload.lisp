;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1982 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module mload)

(declare-top (special $file_search_lisp $file_search_maxima $file_search_demo $loadprint))

(defmfun load-and-tell (filename)
  (loadfile filename t ;; means this is a lisp-level call, not user-level.
	    $loadprint))
       
(defun errset-namestring (x)
  (let ((errset nil))
    (errset (pathname x) nil)))

(defmfun $filename_merge (&rest file-specs)
  (when (or (null file-specs) (cddr file-specs))
    (wna-err '$filename_merge))
  (setq file-specs (mapcar #'macsyma-namestring-sub file-specs))
  (pathname (if (null (cdr file-specs))
                (car file-specs)
                (merge-pathnames (cadr file-specs) (car file-specs)))))

(defun macsyma-namestring-sub (user-object)
  (if (pathnamep user-object) user-object
      (let* ((system-object
	      (cond ((and (atom user-object) (not (symbolp user-object)))
		     user-object)
		    ((atom user-object)	;hence a symbol in view of the
		     (print-invert-case (fullstrip1 user-object))) ; first clause
		    (($listp user-object)
		     (fullstrip (cdr user-object)))
		    (t
		     (merror (intl:gettext "filename_merge: unexpected argument: ~M") user-object))))
	     (namestring-try (errset-namestring system-object)))
	(if namestring-try (car namestring-try)
	    ;; know its small now, so print on same line.
	    (merror (intl:gettext "filename_merge: unexpected argument: ~:M") user-object)))))

(defmvar $load_pathname nil
  "The full pathname of the file being loaded")

(defun $batchload (filename &aux expr (*mread-prompt* ""))
  (declare (special *mread-prompt* *prompt-on-read-hang*))
  (setq filename ($file_search1 filename '((mlist) $file_search_maxima)))
  (let (($load_pathname filename) (noevalargs nil) (*read-base* 10.))
    (with-open-file (in-stream filename)
      (when $loadprint
	(format t (intl:gettext "~&read and interpret file: ~A~&") (cl:namestring (truename in-stream))))
      (cleanup)
      (newline in-stream)
      (loop while (and
		   (setq  expr (let (*prompt-on-read-hang*) (mread in-stream nil)))
		   (consp expr))
	 do (meval* (third expr)))
      (cl:namestring (truename in-stream)))))

;;returns appropriate error or existing pathname.
;; the second argument is a maxima list of variables
;; each of which contains a list of paths.   This is
;; so users can correct the variable..
(defun $file_search1 (name search-lists &aux lis)
  (if (pathnamep name)
      (setq name (namestring name)))
  (setq lis (apply '$append (mapcar 'symbol-value (cdr search-lists))))
  (let ((res ($file_search name lis)))
    (or res
	(merror (intl:gettext "file_search1: ~M not found in ~A.")
		name
		(string-trim "[]" ($sconcat search-lists))))))

(defmfun $load (filename)
  "This is the generic file loading function.
  LOAD(filename) will either BATCHLOAD or LOADFILE the file,
  depending on wether the file contains Macsyma, Lisp, or Compiled
  code. The file specifications default such that a compiled file
  is searched for first, then a lisp file, and finally a macsyma batch
  file. This command is designed to provide maximum utility and
  convenience for writers of packages and users of the macsyma->lisp
  translator."

  (let ((searched-for
	 ($file_search1 filename
			'((mlist) $file_search_maxima $file_search_lisp  )))
	type)
    (setq type ($file_type searched-for))
    (case type
      (($maxima)
       ($batchload searched-for))
      (($lisp $object)
       ;; do something about handling errors
       ;; during loading. Foobar fail act errors.
       (load-and-tell searched-for))
      (t
       ;; UNREACHABLE MESSAGE: DEFAULT TYPE IS '$OBJECT (SEE $FILE_TYPE BELOW)
       (merror "Maxima bug: Unknown file type ~M" type)))
    searched-for))


(defmvar $file_type_lisp
    (list '(mlist) "l" "lsp" "lisp"))

(defmvar $file_type_maxima
    (list '(mlist) "mac" "mc" "demo" "dem" "dm1" "dm2" "dm3" "dmt"))

(defun $file_type (fil)
  (let ((typ ($pathname_type fil)))
    (cond
      ((member typ (cdr $file_type_lisp) :test #'string=)
       '$lisp)
      ((member typ (cdr $file_type_maxima) :test #'string=)
       '$maxima)
      (t
       '$object))))

(defun $pathname_directory (path)
  (let ((pathname (pathname path)))
    (namestring (make-pathname :directory (pathname-directory pathname)))))

(defun $pathname_name (path)
  (let ((pathname (pathname path)))
    (pathname-name pathname)))

(defun $pathname_type (path)
  (let ((pathname (pathname path)))
    (pathname-type pathname)))
  

(defvar *macsyma-startup-queue* nil)

(declaim (special *mread-prompt*))

;;;; batch & demo search hacks

(defun $batch (filename &optional (demo :batch)
	       &aux tem   (possible '(:demo :batch :test)))
  "giving a second argument makes it use demo mode, ie pause after evaluation
   of each command line"
  (cond ((setq tem (member ($mkey demo) possible :test #'eq))
	 (setq demo (car tem)))
	(t (format t (intl:gettext "batch: second argument must be 'demo, 'batch, or 'test; found: ~A, assume 'batch~%") demo)))

  (setq filename ($file_search1 filename
				(if (eql demo :demo)
				    '((mlist) $file_search_demo )
				    '((mlist) $file_search_maxima ))))
  (cond ((eq demo :test)
	 (test-batch filename nil :show-all t))
	(t
	 (let (($load_pathname filename) (*read-base* 10.))
	   (with-open-file (in-stream filename)
	     (format t (intl:gettext "~%read and interpret file: ~A~%")
		     (truename in-stream))
	     (catch 'macsyma-quit (continue in-stream demo))
	     (namestring in-stream))))))

;; Return true if $float converts both a and b to floats and 

;;   |a - b| <= float_approx_equal_tolerance * min(2^n, 2^m), 

;; where a = af * 2^m, |af| < 1, and m is an integer (similarly for b); 
;; in all other cases, return false. See, Knuth, "The Art of Computer Programming," 3rd edition,
;; page 233.

(defmvar $float_approx_equal_tolerance (* 16 flonum-epsilon))

(defun $float_approx_equal (a b)
  (setq a (if (floatp a) a ($float a)))
  (setq b (if (floatp b) b ($float b)))
  (and
   (floatp a)
   (floatp b)
   (<= (abs (- a b)) (* $float_approx_equal_tolerance 
			(min 
			 (expt 2 (- (second (multiple-value-list (decode-float a))) 1))
			 (expt 2 (- (second (multiple-value-list (decode-float b))) 1)))))))

;; Big float version of float_approx_equal. But for bfloat_approx_equal, the tolerance isn't
;; user settable; instead, it is 32 / 2^fpprec. The factor of 32 is too large, I suppose. But
;; the test suite gives a few errors with a factor of 16. These errors might be due to 
;; float / big float comparisons.

(defun $bfloat_approx_equal (a b)
  (setq a (if ($bfloatp a) a ($bfloat a)))
  (setq b (if ($bfloatp b) b ($bfloat b)))
  (let ((m) (bits))
    (and
     ($bfloatp a)
     ($bfloatp b)
     (setq bits (min (third (first a)) (third (first b))))
     (setq m (mul 32 (expt 2 (- bits)) (min (expt 2 (- (car (last a)) 1)) (expt 2 (- (car (last b)) 1)))))
     (setq m (if (rationalp m) (div (numerator m) (denominator m)) m))
     (eq t (mgqp m (take '(mabs) (sub a b)))))))


;; The first argument 'f' is the expected result; the second argument 
;; 'g' is the output of the test. By explicit evaluation, the expected 
;; result *can* be a CL array, CL hashtable, or a taylor polynomial. Such
;; a test would look something like (yes, it's a silly test)

;;    taylor(x,x,0,2);
;;    ''(taylor(x,x,0,2)
 
(defun approx-alike (f g)
 
  (cond ((floatp f) (and (floatp g) ($float_approx_equal f g)))
	
	(($bfloatp f) (and ($bfloatp g) ($bfloat_approx_equal f g)))
	
	(($taylorp g)
	 (approx-alike 0 (sub (ratdisrep f) (ratdisrep g))))
	
	((atom f) (and (atom g) (equal f g)))
		     
	((op-equalp f 'lambda)
	 (and (op-equalp g 'lambda)
	      (approx-alike-list (mapcar #'(lambda (s) (simplifya s nil)) (margs f))
				 (mapcar #'(lambda (s) (simplifya s nil)) (margs g)))))
	
	((arrayp f)
	 (and (arrayp g) (approx-alike ($listarray f) ($listarray g))))
	
	((hash-table-p f)
	 (and (hash-table-p g) (approx-alike ($listarray f) ($listarray g))))
	
	(($ratp f)
	 (and ($ratp g) (approx-alike (ratdisrep f) (ratdisrep g))))
	
	;; maybe we don't want this.
	((op-equalp f 'mquote)
	 (approx-alike (second f) g))
	 
	;; I'm pretty sure that (mop f) and (mop g) won't signal errors, but
	;; let's be extra careful.

	((and (consp f) (consp (car f)) (consp g) (consp (car g))
	      (or (approx-alike (mop f) (mop g)) 
		  (and (symbolp (mop f)) (symbolp (mop g))
		       (approx-alike ($nounify (mop f)) ($nounify (mop g)))))
	      (approx-alike-list (margs f) (margs g))))
	
	(t nil)))

(defun approx-alike-list (p q)
  (cond ((null p) (null q))
	((null q) (null p))
	(t (and (approx-alike (first p) (first q)) (approx-alike-list (rest p) (rest q))))))

(defun simple-equal-p (f g)
  (approx-alike (simplifya f nil) (simplifya g nil)))

(defun batch-equal-check (expected result)
  (let ((answer (catch 'macsyma-quit (simple-equal-p expected result))))
    (if (eql answer 'maxima-error) nil answer)))

(defvar *collect-errors* t)

(defun test-batch (filename expected-errors
			    &key (out *standard-output*) (show-expected nil)
			    (show-all nil) (showtime nil))

  (let ((result) (next-result) (next) (error-log) (all-differences nil) ($ratprint nil) (strm)
	(*mread-prompt* "") (*read-base* 10.)
	(expr) (num-problems 0) (tmp-output) (save-output) (i 0)
	(start-run-time 0) (end-run-time 0)
	(start-real-time 0) (end-real-time 0)
	(test-start-run-time 0) (test-end-run-time 0)
	(test-start-real-time 0) (test-end-real-time 0))
    
    (cond (*collect-errors*
	   (setq error-log
		 (if (streamp *collect-errors*) *collect-errors*
		   (handler-case
		       (open (alter-pathname filename :type "ERR") :direction :output :if-exists :supersede)
		     #-gcl (file-error () nil)
		     #+gcl (cl::error () nil))))
	   (when error-log
	     (format t (intl:gettext "~%batch: write error log to ~a") error-log)
	     (format error-log (intl:gettext "~%/* Maxima error log from tests in ~A") filename)
	     (format error-log " */~2%"))))
 
    (unwind-protect 
	(progn
	  (setq strm (open filename :direction :input))
	  (setq start-real-time (get-internal-real-time))
	  (setq start-run-time (get-internal-run-time))
	  (while (not (eq 'eof (setq expr (mread strm 'eof))))
	    (incf num-problems)
	    (incf i)
	    (setf tmp-output (make-string-output-stream))
	    (setf save-output *standard-output*)
	    (setf *standard-output* tmp-output)
	  
	    (unwind-protect
		(progn
		  (setq test-start-run-time (get-internal-run-time))
		  (setq test-start-real-time (get-internal-real-time))
		  (setq result (meval* `(($errcatch) ,(third expr))))
		  (setq result (if ($emptyp result) 'error-catch (second result)))
		  (setq test-end-run-time (get-internal-run-time))
		  (setq test-end-real-time (get-internal-real-time))
		  (setq $% result))
	      (setf *standard-output* save-output))

	    (setq next (mread strm 'eof))
	    (if (eq next 'eof) (merror (intl:gettext "batch: missing expected result in test script.")))
	  
	    (setq next-result (third next))
	    (let* ((correct (batch-equal-check next-result result))
		   (expected-error (member i expected-errors))
		   (pass (or correct expected-error)))
	      (when (or show-all (not pass) (and correct expected-error)
			(and expected-error show-expected))
		(format out (intl:gettext "~%********************** Problem ~A ***************") i)
		(format out (intl:gettext "~%Input:~%"))
		(displa (third expr))
		(format out (intl:gettext "~%~%Result:~%"))
		(format out "~a" (get-output-stream-string tmp-output))
		(displa $%)
		(when (eq showtime '$all)
		  (format out (intl:gettext "~%Time:  ~,3F sec (~,3F elapsed)")
			  (float (/ (- test-end-run-time test-start-run-time)
				    internal-time-units-per-second))
			  (float (/ (- test-end-run-time test-start-run-time)
				    internal-time-units-per-second)))))
	      (cond ((and correct expected-error)
		     (format t (intl:gettext "~%... Which was correct, but was expected to be wrong due to a known bug in~% Maxima.~%")))
		    (correct
		     (if show-all (format t (intl:gettext "~%... Which was correct.~%"))))
		    ((and (not correct) expected-error)
		     (if (or show-all show-expected)
			 (progn
			   (format t (intl:gettext "~%This is a known error in Maxima. The correct result is:~%"))
			   (displa next-result))))
		    (t (format t (intl:gettext "~%This differed from the expected result:~%"))
		       (push i all-differences)
		       (displa next-result)
		       (cond ((and *collect-errors* error-log)
			      (format error-log (intl:gettext "/* Problem ~A */~%") i)
			      (mgrind (third expr) error-log)
			      (list-variable-bindings (third expr) error-log)
			      (format error-log ";~%")
			      (format error-log (intl:gettext "/* Erroneous Result?:~%"))
			      (mgrind result error-log) (format error-log " */ ")
			      (terpri error-log)
			      (format error-log (intl:gettext "/* Expected result: */~%"))
			      (mgrind next-result error-log)
			      (format error-log ";~%~%"))))))))
      (close strm))
    (setq end-run-time (get-internal-run-time))
    (setq end-real-time (get-internal-real-time))
    (cond (error-log
	   (or (streamp *collect-errors*)
	       (close error-log))))
    (let
      ((expected-errors-trailer
	(if (or (null expected-errors) (= (length expected-errors) 0))
	    ""
	    (format nil (intl:gettext " (not counting ~a expected errors)") (length expected-errors))))
       (time (if showtime
		 (format nil (intl:gettext "   using ~,3F seconds (~,3F elapsed).~%")
			 (float (/ (- end-run-time start-run-time) internal-time-units-per-second))
			 (float (/ (- end-real-time start-real-time) internal-time-units-per-second)))
		 "")))
      (cond ((null all-differences)
	     (format t (intl:gettext "~a/~a tests passed~a~%~A")
		     num-problems num-problems
		     expected-errors-trailer
		     time)
	     (values '((mlist)) num-problems))
	    (t
	     (format t (intl:gettext "~%~a/~a tests passed~a~%~A")
		     (- num-problems (length all-differences)) num-problems expected-errors-trailer
		     time)
	     (let ((s (if (> (length all-differences) 1) "s" "")))
	       (format t (intl:gettext "~%The following ~A problem~A failed: ~A~%")
		       (length all-differences) s (reverse all-differences)))
	     (values `((mlist) ,filename ,@(reverse all-differences)) num-problems))))))
       
;;to keep track of global values during the error:
(defun list-variable-bindings (expr &optional str &aux tem)
  (loop for v in(cdr ($listofvars  expr))
    when (member v $values :test #'equal)
    collecting (setq tem`((mequal) ,v ,(meval* v)))
    and
    do (cond (str (format str ",")(mgrind tem str)))))

;;in init_max
;; name = foo or foo.type or dir/foo.type or dir/foo 
;; the empty parts are filled successively from defaults in templates in
;; the path.   A template may use multiple {a,b,c} constructions to indicate
;; multiple possiblities.  eg foo.l{i,}sp or foo.{dem,dm1,dm2}
(defun $file_search (name &optional paths)
  (if (and (symbolp name)
	   (member (char (symbol-name name) 0) '(#\$)))
      (setq name (subseq (print-invert-case name) 1)))
  (if (symbolp name)  (setf name (string name)))
  (if (probe-file name) (return-from $file_search name))
  (or paths (setq paths ($append $file_search_lisp  $file_search_maxima
				 $file_search_demo)))
  (atomchk paths '$file_search t)
  (new-file-search (string name) (cdr paths)))

(defun new-file-search (name template)
  (cond ((probe-file name))
	((atom template)
	 (let ((lis (loop for w in (split-string template "{}")
			  when (null (position #\, w))
			  collect w
			  else
			  collect (split-string w ","))))
	   (new-file-search1 name "" lis)))
	(t
	 (let ((temp nil))
	   (loop for v in template
		 when (setq temp (new-file-search name v))
		 do (return temp))))))

(defun new-file-search1 (name begin lis)
  (cond ((null lis)
	 (let ((file (namestring ($filename_merge begin name))))
	   (if (probe-file file) file nil)))
	((atom (car lis))
	 (new-file-search1 name
			   (if begin
			       ($sconcat begin (car lis)) (car lis))
			   (cdr lis)))
	(t (loop for v in (car lis) with tem
		  when (setq tem  (new-file-search1 name begin (cons v (cdr lis))))
		  do (return tem)))))

(defun save-linenumbers (&key (c-lines t) d-lines (from 1) (below $linenum) a-list
			 (file  "/tmp/lines")
			 &aux input-symbol (linel 79))
  (cond ((null a-list) (setq a-list (loop for i from from below below collecting i))))
  (with-open-file (st file :direction :output)
    (format st "/* -*- Mode: MACSYMA; Package: MACSYMA -*- */")
    (format st "~%~%       /*    ~A     */  ~%"
	    (let ((tem (cdddr
				(multiple-value-list (get-decoded-time)))))
		      (format nil "~a:~a:~a" (car tem) (cadr tem) (caadr tem))))
    (loop for i in a-list
	   when (and c-lines (boundp (setq input-symbol (intern (format nil "$~A~A" '#:c i)))))
	   do
	   (format st "~% C~3A;  "   i)
	   (mgrind (symbol-value input-symbol) st)
	   (format st ";")
	   when (and d-lines
		     (boundp (setq input-symbol (intern (format nil "$~A~A" '#:d i)))))
	   do
	   (format st "~% D~3A:  "   i)
	   (mgrind (symbol-value input-symbol) st)
	   (format st "$"))))


(defun $printfile (file)
  (setq file ($file_search1 file '((mlist) $file_search_usage)))
  (with-open-file (st file)
    (loop
       with tem
       while (setq tem (read-char st nil 'eof)) 
       do
       (if (eq tem 'eof) (return t))
       (princ tem))
    (namestring file)))

  
  
(defmvar $testsuite_files nil)

(defvar *maxima-testsdir*)

(defun intersect-tests (tests)
  ;; If TESTS is non-NIL, we assume it's a Maxima list of (maxima)
  ;; strings naming the tests we want to run.  They must match the
  ;; file names in $testsuite_files.  We ignore any items that aren't
  ;; in $testsuite_files.
  (flet ((remove-dollarsign (x)
	   ;; Like stripdollar, but less heavy
	   (if (symbolp x)
	       (subseq (maxima-string x) 1)
	       x)))
    (mapcar #'remove-dollarsign
	    (cond (tests
		   (let ((results nil))
		     ;; Using INTERSECTION would be convenient, but
		     ;; INTERSECTION can return the result in any
		     ;; order, and we'd prefer that the order of the
		     ;; tests be preserved.  CMUCL and CCL returns the
		     ;; intersection in reverse order.  Clisp produces
		     ;; the original order.  Fortunately, this doesn't
		     ;; have to be very fast, so we do it very naively.
		     (dolist (test (mapcar #'remove-dollarsign (cdr tests)))
		       (let ((matching-test (find test (cdr $testsuite_files)
						  :key #'(lambda (x)
							   (maxima-string (if (listp x)
									      (second x)
									      x)))
						  :test #'string=)))
			 (when matching-test
			   (push matching-test results))))
		     (nreverse results)))
		  (t
		   (cdr $testsuite_files))))))

(defun run-testsuite (&key display_known_bugs display_all tests time)
  (declare (special $file_search_tests))
  (let ((test-file)
	(expected-failures))
    ;; Allow only T and NIL for display_known_bugs and display_all
    (unless (member display_known_bugs '(t nil))
      (merror (intl:gettext "run_testsuite: display_known_bugs must be true or false; found: ~M") display_known_bugs))
    (unless (member display_all  '(t nil))
      (merror (intl:gettext "run_testsuite: display_all must be true or false; found: ~M") display_all))
    (unless (member time '(t nil $all))
      (merror (intl:gettext "run_testsuite: time must be true, false, or all; found: ~M") time))
    
    (setq *collect-errors* nil)
    (unless $testsuite_files
      (let ((*read-base* 10.)) (load (concatenate 'string *maxima-testsdir* "/" "testsuite.lisp"))))
    (let ((error-break-file)
	  (testresult)
	  (tests-to-run (intersect-tests (cond ((consp tests) tests)
					       (tests (list '(mlist) tests)))))
	  (test-count 0)
	  (total-count 0)
	  (error-count 0))
      (flet
	  ((testsuite ()
	     (loop with errs = 'nil
		   for testentry in tests-to-run
		   do (if (atom testentry)
			  (progn
			    (setf test-file testentry)
			    (setf expected-failures nil))
			  (progn
			    (setf test-file (second testentry))
			    (setf expected-failures (cddr testentry))))
		      (format t
			      (intl:gettext "Running tests in ~a: ")
			      (if (symbolp test-file)
				  (subseq (print-invert-case test-file) 1)
				  test-file))
		      (or
			(errset
			  (progn
			    (multiple-value-setq (testresult test-count)
			      (test-batch ($file_search test-file $file_search_tests)
					  expected-failures :show-expected display_known_bugs
					  :show-all display_all :showtime time))
			    (setf testresult (rest testresult))
			    (incf total-count test-count)
			    (when testresult
			      (incf error-count (length (cdr testresult)))
			      (setq errs (append errs (list testresult))))))
			(progn
			  (setq error-break-file (format nil "~a" test-file))
			  (setq errs
				(append errs
					(list (list error-break-file "error break"))))
			  (format t
				  (intl:gettext "~%Caused an error break: ~a~%")
				  test-file)))
		   finally (cond
			     ((null errs)
			      (format t
				      (intl:gettext
				       "~%~%No unexpected errors found out of ~:d tests.~%")
				      total-count))
			     (t
			      (format t (intl:gettext "~%Error summary:~%"))
			      (mapcar
			       #'(lambda (x)
				   (let ((s (if (> (length (rest x)) 1) "s" "")))
				     (format t
					     (intl:gettext
					      "Error~a found in ~a, problem~a:~%~a~%")
					     s
					     (first x)
					     s
					     (sort (rest x) #'<))))
			       errs)
			      (format t
				      (intl:gettext
				       "~&~:d test~p failed out of ~:d total tests.~%")
				      error-count
				      error-count
				      total-count))))))
      (time (testsuite)))))
  '$done)

;; Convert a list of Maxima "keyword" arguments into the corresponding
;; list of Lisp keyword arguments.  Maxima options look like
;; opt1=val1, opt2=val2, etc.  These are converted to :opt1 val1 :opt2
;; val2, which can be directly given to a Lisp function with those
;; keyword arguments.  If VALID-KEYWORDS is specified, only those
;; (Maxima) keywords will be recognized.  Unrecognized ones will
;; signal an error.  If VALID-KEYWORDS is not specified, then all
;; keywords will be converted, and it is up to the Lisp routine to
;; decide what to do with the extra keyword arguments.
(defun lispify-maxima-keyword-options (options &optional valid-keywords)
  ;; options looks like (((mequal) $opt1 val1) ((mequal) $opt2 val2) ...)
  ;;
  ;; Convert to a new list that looks like (:opt1 val1 :opt2 val2 ...)
  ;;
  (unless (listp options)
    ;; UNREACHABLE MESSAGE: OPTIONS IS A &REST ARGUMENT TO $RUN_TESTSUITE SO IT MUST PASS LISTP
    (merror "Invalid Maxima keyword options: ~M" options))
  (when (every #'(lambda (o)
		   ;; Make sure every option has the right form.
		   (let ((ok (and (listp o)
				  (= (length o) 3)
				  (eq (caar o) 'mequal))))
		     (unless ok
		       (merror (intl:gettext "Badly formed keyword option: ~M") o))
		     ok))
		 options)
    (mapcan #'(lambda (o)
	      (destructuring-bind (mequal opt val)
		  o
		(declare (ignore mequal))
		(if (or (null valid-keywords)
			(member opt valid-keywords))
		    (flet ((keywordify (x)
			     (intern (subseq (symbol-name x) 1) :keyword)))
		      (list (keywordify opt) val))
		    (merror (intl:gettext "Unrecognized keyword: ~M") opt))))
	    options)))

(defun $run_testsuite (&rest options)
  (apply #'run-testsuite
	 (lispify-maxima-keyword-options options '($display_all $display_known_bugs $tests $time))))
