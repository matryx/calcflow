;; It seems that the current liszt does not compile the
;; function $readvexp correctly
;; The interpretive version works -- pwang 10/17/88

(defun $readvexp (in)
  ;  $readvexp is the parser for expressions and statements        ;
  ;  inside "<<" and ">>" within the template file.                ;
  ;                                                                ;
  ;  "/*  ...  */" can be used for comments inside "<<" and ">>".  ;
  (prog (test oldst st iport)
      (setq iport (cdr in))
   loop (setq test (tyi iport))
   c    (cond ((and (equal test #\*)
		    st
		    (equal (car st) #\/))
	        (do ((ch1 (tyi iport) ch2) (ch2))
		   ((or (and (equal ch1 #\*) (equal ch2 #\/))
			(equal ch2 *eof*))
		    (setq st (cdr st)))
		   (setq ch2 (tyi iport)))
	        (go loop))
	      ((member test '(#\space #\tab #\linefeed))
	       (cond ((null st) (go loop))))
	      ((and (equal test #\>)
		    st
		    (equal (car st) #\>))
	       (setq *vexptrm test)
	       (go d))
	      ((member test '(#\; #\$ *eof*))
	       (setq *vexptrm test)
	       (go d))
	      ((equal test #\\)
	       (setq st (cons test st)
		     test (tyi iport))))
	(setq st (cons test st))
	(go loop)
   d    (cond ((and (null st)
		    (not (member *vexptrm '(#\linefeed *eof* #\>))))
	       (go loop)))
	(setq oldst st)
	(cond ((null st) (return nil))
	      ((setq test (let (*prompt-on-read-hang*) (mread iport nil))) (return (third test))))
	(setq test (tyi iport))
	(go c)))
