;;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;returns the I'th open of the blowup of the FIRSTK coord of ZOPEN
(defun iblowup (zopen i firstk &aux ichart pss qss ineq answ)
  (setq ichart   (ichart i firstk (zopen-dim zopen)))
  (setq pss  (compose-rmap (zopen-coord ichart)
			   (zopen-coord zopen)))
  (setq qss
	(compose-rmap
		 (zopen-inv zopen)
		 (zopen-inv ichart)))
  (setq ineq (function-numerator (apply-rmap (zopen-inv ichart) (zopen-inequality zopen))))
  (setq answ (zl-copy-structure zopen zopen- coord pss inv qss inequality ineq))
  answ)

(defun s-blow (  simp-pls open-number component-number)
  (multiple-value-bind
    (pls empty-opens codim)
      (normalize-pls simp-pls open-number component-number)
    (show empty-opens codim)
    (simplify-svar-ldata  (blowup-sheaf pls codim  :opens-not-to-blowup empty-opens))))

(defun high-dimensional-component (pls &aux tem ld-num)
  (loop for open-number from 0
	for lis-dat in (pls-data pls)
	with min-open-number = 0
	with prev-min = 100
	do (loop for v in lis-dat
		 for ld-number from 0
		 when (< (setq tem (length (ldata-eqns v))) prev-min)
		 do (setq min-open-number open-number ld-num ld-number)
		 (setq prev-min tem))
	finally (return (values (list min-open-number ld-num) prev-min))))

(defmacro vformat (&rest l)
  `(cond (*verbose* (format t ,@ l))
	 (t nil)))

(defmacro vdes (&rest l)
  `(cond (*verbose* (des ,@ l))
	 (t nil)))

(defun blowup-pls-using-high-dimensional-components  (pls n &key (one-open t) already-blown-up &aux (current-pls pls) op codim)
  (block sue
    (loop for i from n  while (pls-data current-pls)
       with new-pls = current-pls
       with red-pls = current-pls
       with first-time = t
       when (null first-time)
       do
	 (vformat t "reduced  pls:")
	 (vdes red-pls)
	 (vformat t "homogeneous simplified pls:")
       ;;if keeping more than one open the current-pls will be simplified inside blowup
       ;;since that way we only simplify opens blownup.
	 (cond (one-open  (setq current-pls (simplify-svar-homogeneous current-pls))))
	 (vdes current-pls)
	 (cond ((null (pls-data red-pls))
		(return (list blew-up i))))
	 (multiple-value (op codim) (high-dimensional-component red-pls))
	 (show op codim)
	 (format t "~%The ~A blowup: the ~A component of the ~A open of codimension ~A."
		 i (second op) (car op) codim)
	 (setq new-pls (blowup-pls current-pls (nth (car op) (pls-opens red-pls))
				   (ldata-eqns  (nth (second op)
						     (nth (car op) (pls-data red-pls))))
				   :simplify-homogeneous (null one-open)))

       and
       collecting (list (format nil "Before the ~A blowup the homogeneous equations on one of the nontrivial opens looked like :" i)
			current-pls) into blew-up
       and
       collecting (list (format nil "~%The ~A blowup was by the ~A ldata  of the following open:" i (second op)) (open-sub-scheme red-pls (car op)))
       into blew-up
       do
	 (setq first-time nil)
	 (loop for ii from 0
	    for ope in (pls-opens new-pls)
	    do
	      (setq red-pls (simplify-svar-ldata (open-sub-scheme new-pls ii)))
	      (show ii (length (pls-data red-pls)))
	      (format t "Open history: ~A" (zopen-history  (car (pls-opens current-pls))))
	    when (pls-data red-pls)
	    do
	      (cond (one-open (setq current-pls (open-sub-scheme new-pls ii)))
		    (t (setq current-pls new-pls)))
	      (format t "~%Taking the ~A open with history " ii)
	      (return 'done)
	    else do (format t "~%The ~A open is empty")
	    finally (return-from sue (append already-blown-up blew-up)))
       finally (return (list (append already-blown-up blew-up) i)))))

;(defun blowup-pls-using-choices (pls n &key (query-user t) (one-open t) already-blown-up
;				 (number-to-reduce 3) &aux (current-pls pls) choice
;				   )
;  (loop named sue for i from n  while (pls-data current-pls)
;	with new-pls =  current-pls
;;	with red-pls = current-pls
;	with first-time = t
;	   when (null first-time)
;	   do
;          (cond ((null query-user)
;		 (vformat t "Going to blowup wrt to reduced  pls:")
;		 ;(vdes red-pls)))
;	   (vformat t "homogeneous simplified pls:")))
;
;	   ;;if keeping more than one open the current-pls will be simplified inside blowup
;	   ;;since that way we only simplify opens blownup.
;	   (cond (one-open  (setq current-pls (simplify-svar-homogeneous current-pls))))
;	   (vdes current-pls)
;;	   (cond ((null (pls-data red-pls))
;;		  (return (list blew-up i))))
;;	   (multiple-value ( op codim)
;;	     (high-dimensional-component red-pls))
;;	   (show op codim)
;           (cond ((null query-user)
;		  (format t "~%The ~A blowup: The coordinates and the equations" i)
;		  (des (second choice)) (des (third choice))))
;	   (setq new-pls (blowup-pls current-pls (second choice)
;				     (ldata-eqns (third choice))
;				     :simplify-homogeneous t))
;	   and
;	   collecting (list (format nil "Before the ~A blowup the homogeneous equations ~
;             on one of the nontrivial opens looked like :" i)current-pls)
;	   into blew-up
;	   and
;	   collecting (list
;			(format nil "~%The ~A blowup was by :"
;				i ) (second choice) (third choice))
;
;	   into blew-up
;
;	do
;	(setq first-time nil)
;	(setq choice
;	      (provide-some-choices-to-blowup new-pls :query-user query-user :number-to-reduce number-to-reduce))
;	(cond ((null (third choice))(return-from sue (append already-blown-up blew-up))))
;	(cond (one-open
;	       (loop for op in (pls-opens new-pls)
;		     for op-num from 0
;		     do (setq current-op-number op-num)
;		     when (zopen-equal op (second choice))
;		     do (setq current-pls (open-sub-scheme new-pls op-num))
;		     (return 'done)
;		     (finally (merror "could not find the open"))))
;	      (t       (setq current-pls new-pls)))
;	finally (return-from sue  (append already-blown-up blew-up))))
(defvar *blew-up* nil)
(defvar *previous-blowups* nil)



(defun eliminate-empty-and-larger-opens ( pls &aux pairs)
  (loop for v in (pls-opens pls)
	for lis-dat in (pls-data pls)
	when lis-dat
	collecting (cons v lis-dat) into paired-dat
	finally
;	(setq paired-dat  (eliminate-smaller paired-dat :key 'car
;					       :test 'zopen-special-subset))
	(return (construct-pre-ldata-sheaves :opens (mapcar 'car paired-dat)
					     :data (mapcar 'cdr paired-dat)))))
(defvar *query-user* t)

(defun blowup-pls-using-choices (pls n &key (query-user t) (one-open t) already-blown-up
				 (number-to-reduce 3) &aux (current-pls pls) choice
				 current-op-number
				  choices )
  (setq *query-user* query-user)
  (loop named sue for i from n  while (pls-data current-pls)
	with new-pls =  current-pls
	with first-time = t
	   when (null first-time)
	   do
	   (cond ((null query-user)
		 (vformat t "Going to blowup wrt to reduced  pls:")
		 ;(vdes red-pls)))
	   (vformat t "homogeneous simplified pls:")))

	   ;;if keeping more than one open the current-pls will be simplified inside blowup
	   ;;since that way we only simplify opens blownup.
;	   (cond (one-open  (setq current-pls (simplify-svar-homogeneous current-pls))))
	   (vdes current-pls)
	   (loop  for vch on  choices
		  do (setq choice (car vch))
		  (setq new-pls nil)
		  (cond ((null query-user)
			 (format t "~%The ~A blowup: The coordinates and the equations" i)
			 (des (second choice)) (des (third choice))))
		  (catch 'new-choice
		    (setq new-pls (blowup-pls current-pls (second choice)
					      (ldata-eqns (third choice))
					      :simplify-homogeneous t)))
		  (cond (new-pls
			 (setq new-pls (eliminate-empty-and-larger-opens new-pls))
			 (return 'done)))
		  finally (fsignal "Ran out of choices"))
	   and
	   collecting (list
			(format nil "~%The ~A blowup was by equations on the ~A open :"
				i current-op-number ) (second choice) (third choice))
	   into blew-up
	   and
	   collecting (list (format nil "After the ~A blowup the homogeneous equations ~
	     on one of the nontrivial opens looked like :" i)new-pls)
	   into blew-up
	   and do (setq *blew-up* blew-up)
	do (push *blew-up* *previous-blowups*) (setq *blew-up* blew-up)
;	(user:write-object "Alonzo:wfs>blew-up.tem" (nthcdr (- (length *blew-up*) 2) *blew-up*))
	(setq first-time nil)
	(setq choices
	      (provide-some-choices-to-blowup new-pls
					      :query-user query-user
					      :number-to-reduce number-to-reduce))
	(cond ((null choices)(return-from sue (append already-blown-up blew-up))))
	(setq choice (car choices))
	(loop for op in (pls-opens new-pls)
	      for op-num from 0
	      do (setq current-op-number op-num)
	      when (zopen-special-subset (second choice) op)
	      do  (return 'done)
	      (finally (fsignal "could not find the open")))
	(cond (one-open	      (setq current-pls (open-sub-scheme new-pls
								 current-op-number)))
	      (t       (setq current-pls new-pls)))
	finally (return-from sue  (append already-blown-up blew-up))))
(defun restart-blowup  ( &key part use-blewup one-open query-user (after 0) (number-to-reduce 3)  &aux pls)
  (cond (part (setq pls (second part))
	      (blowup-pls-using-choices pls after :query-user query-user :one-open one-open
					:already-blown-up (list part) :number-to-reduce
					number-to-reduce))

	(use-blewup
	 (let ((las (nth (1- (length *blew-up*)) *blew-up*))
	       (blas (nth (- (length *blew-up*) 2) *blew-up*))
	       (n (quotient (length *blew-up*) 2)))
	   (cond ((search "after" (car las) :test #'char-equal) (setq pls (second las)))
		 (t (setq pls (second blas)) (setq *blew-up* (butlast *blew-up*))))
	   (blowup-pls-using-choices pls n :query-user query-user :one-open one-open
				     :number-to-reduce number-to-reduce
				     :already-blown-up *blew-up*)))))


;(defun restart-blowup  (&key one-open query-user after &aux pls)
;  (let ((las (nth  (1- (length *blew-up*)) *blew-up*))
;	(blas (nth  (- (length *blew-up*) 2) *blew-up*))
;	(n (quotient (length *blew-up*) 2)))
;    (cond ((search "after" (car las) :test #'char-equal) (setq pls (second las)))
;	  (t (setq pls (second blas)) (setq *blew-up* (butlast *blew-up*))))
;    (blowup-pls-using-choices pls n :query-user query-user :one-open one-open
;				     :already-blown-up *blew-up*)))
(defun gen-pcomplexity (lis)
  (cond ((null lis) 0)
	((polynomialp lis) (pcomplexity lis))
	((rational-functionp lis) (+ (pcomplexity (num lis))
				     (pcomplexity (denom lis))))
	((listp lis) (loop for v in lis summing (gen-pcomplexity v)))
	(t (merror "don't know how complex"))))
;
;(defmacro user-supply (var)
;  `(let ((*print-level* 3) ( prinlength 3)  .new.)
;     (loop
;       do
;       (format t "~%The value of ~A is ~A ." ',var ,var)
;       (format t "~%Supply a form to evaluate to use for ~A or 'keep to keep same :" ',var)
;       (setq .new. (eval (read)))
;       (cond ((eq .new. 'keep)(return ,var))
;	     (t
;	      (format t "~%Use ~A?"  .new.)
;	      (cond ((y-or-n-p )(return (setq ,var .new.)))))))))
;


(defvar *reduced-history* nil)
;;returns an ordered list of choices..
(defun provide-some-choices-to-blowup (new-pls &key (query-user t) (number-to-reduce 3)
				       &aux   component-number an-open-number a-red-pls
				       n len red-pls choices red-choices  pcomp min-pcomp some-red-pls the-min comp-eqns min-comp number-eqns opens-not-to-try the-max
				       opens-to-try			   (check-the-worst t) where-complex where-the-max   answ)
  (cond
    ((or (null  query-user)
	 (yes-or-no-p " Simplify and provide choices of open and equation "))
     (loop for lis-ld in (pls-data new-pls)
	   when lis-ld
	   collecting (setq len (length (ldata-eqns (car lis-ld)))) into tem
	   and
	   collecting (setq pcomp
			    (gen-pcomplexity (ldata-eqns (car lis-ld))))
	   into tem1
	   else collecting 0 into tem
	   and collecting 0 into tem1
	   when lis-ld
	   minimize len into the-min1
	   and
	   maximize len into the-max1
	   and
	   minimize pcomp into min-pcomp1
	   and
	   maximize pcomp into max-pcomp1
	   finally (setq the-min the-min1 min-pcomp min-pcomp1 number-eqns tem
			 comp-eqns tem1 the-max the-max1)
	   (setq where-complex (find-position-in-list max-pcomp1 comp-eqns)))
     (vformat t "%The number of equations are ~A and their complexity is ~A"
	      number-eqns comp-eqns)
     (setq where-the-max (find-position-in-list the-max number-eqns))
     (loop for v in number-eqns
	   for w in comp-eqns
	   for lis-ld in (pls-data new-pls)
	   for op in (pls-opens new-pls)
	   for i from 0
	   when (not (zerop v))
	   do (format t "~%On open ~A there are ~A equations of complexity ~A ." i v w)
	   when (equal v the-min)
	   do (push (list (format nil "Least homogeneous equations")
			  op (car lis-ld) ) choices)
	   when (eql w min-comp)
	   do (push  (list (format nil "Least complex equations")
			   op (car lis-ld) ) choices))
     (cond(query-user (user-supply check-the-worst)))
     (cond (query-user (user-supply opens-not-to-try)
		       (user-supply opens-to-try)))

	   (loop for ii from 0
		 for op in (pls-opens new-pls)
		 when
		 (and (null (member ii opens-not-to-try :test #'equal))
		      (or
			(< (length some-red) number-to-reduce) (member ii opens-to-try :test #'equal)))
		 do  (push ii opens-not-to-try) (format t "~%Reducing the ~A open" ii)
		 (show ii opens-not-to-try (member ii opens-not-to-try :test #'equal)
		       (null (member ii opens-not-to-try :test #'equal)))
		 (setq red-pls
		       (simplify-svar-ldata (open-sub-scheme new-pls ii)))
		 (push (car (make-component-history red-pls))(zopen-history op))
		 and
		 when (null (pls-data red-pls))
		 do (setf (nth  ii (pls-data new-pls)) nil)

		 else collecting red-pls into some-red
		 finally
		 (show the-max opens-not-to-try where-the-max)
		 (cond ((and check-the-worst (null (mem where-the-max opens-not-to-try :test #'eq)))
			(setq some-red-pls
			      (cons (simplify-svar-ldata
				      (open-sub-scheme new-pls where-the-max)) some-red)))
		       (t (setq some-red-pls some-red))))
	   (setq red-choices
		 (loop for red-pls in some-red-pls
		       appending
		       (loop for lis-ld in (pls-data red-pls)
			     for op in (pls-opens red-pls)
			     appending
			     (loop for ld in lis-ld
				   collecting (list "Reduced ld" op ld)))))
	   (setq red-choices (sort red-choices
				   #'(lambda (u v) (order-ldata (third u) (third v)))))
	   (setq choices (append red-choices choices))
	   (push some-red-pls *Reduced-history*)
	   (cond ( query-user
		  (loop while (yes-or-no-p "Show the reduced pls's?")
			do (des some-red-pls))))
	   (cond (query-user
		  (loop named kay for v in choices while (y-or-n-p "show next?")
			for i from 0
			do (format t "~3% Number**** ~A :" i)
			(show (length v))
			(loop for ww in v do (des ww))
			finally
			(loop do
			      (format  t "~%Type the number of the  one do you want:")
			      (setq n (read))
			      (cond ((not (and  (numberp n) (< n (length choices)))) nil)
				    (t (des (nth n choices))
				       (cond ((yes-or-no-p "~%Choosing this one?")
					      (return-from kay
						(cons (nth n choices)
						      (delete-nth n choices))))
					     (t nil)))))

			))
		 (t  choices)))
     (t
       (loop do
	     (format  t "enter a form to evaluate giving (list nil open ldata-to-blowup) ~
	     or T to enter pls and component numbers: ")

	     (setq answ   (eval (read)))
	     (cond ((atom answ)
		    (user-supply a-red-pls)
	     (user-supply an-open-number)
	     (user-supply component-number)
		    (setq answ
			  (list nil
				(nth an-open-number (pls-opens  a-red-pls))
				(nth component-number (nth an-open-number (pls-data
									 a-red-pls)))))))
	     (iassert (and (eq (car (second answ)) 'zopen)))
	     (iassert (and (eq (car (third answ)) 'ldata)))
	     (des answ)
	     (cond ((y-or-n-p "Use it?")
		    (return (list answ))))))))


(defun sort-the-reduced-history (some-red-pls  &aux red-choices)
  (setq red-choices
	(loop for red-pls in some-red-pls
	   appending
	     (loop for lis-ld in (pls-data red-pls)
		    for op in (pls-opens red-pls)
		    appending
		    (loop for ld in lis-ld collecting (list "Reduced ld" op ld)))))
  (setq red-choices (sort red-choices #'(lambda (u v) (order-ldata (third u) (third v))))))

(defun order-ldata (u v)
  (cond ((< (length (ldata-eqns u))
	    (length (ldata-eqns  v))))
	((= (length (ldata-eqns u))
	    (length (ldata-eqns  v)))
	 (< (gen-pcomplexity (ldata-eqns u))
	    (gen-pcomplexity (ldata-eqns v))))
	(t nil)))


;;;the following works to blow up following one open thru
;(defun blowup-pls-using-high-dimensional-components (pls n &aux (current-pls pls)
;						     op codim )
;  (loop named sue for i from n  while (pls-data current-pls)
;	with new-pls =  current-pls
;	with red-pls = current-pls
;	with first-time = t
;	when (null first-time)
;	   do
;	   (format t "reduced  pls:")
;	   (des red-pls)
;	   (format t "homogeneous simplified pls:")
;	   (setq current-pls (simplify-svar-homogeneous current-pls))
;	   (des current-pls)
;	   (cond ((null (pls-data red-pls))
;		  (return (list blew-up i))))
;	   (multiple-value ( op codim)
;	     (high-dimensional-component red-pls))
;	   (show op codim)
;	   (format t "~%The ~A blowup: the ~A component of the ~A open of codimension ~A."
;		   i (second op) (car op) codim)
;
;	   (setq new-pls (blowup-pls current-pls (nth (car op) (pls-opens red-pls))
;				     (nth (second op) (nth (car op) (pls-data red-pls)))))
;
;	   and
;	   collecting (list (format nil "Before the ~A blowup the homogeneous equations on one of the nontrivial opens looked like :" i)current-pls) into blew-up
;	   and
;	   collecting (list
;			(format nil "~%The ~A blowup was by the ~A ldata  of the following open:" i (second op)) (open-sub-scheme red-pls (car op)))
;	   into blew-up
;	do
;	(setq first-time nil)
;	(loop for ii from 0
;	      for ope in (pls-opens new-pls)
;              do
;	      (setq red-pls (simplify-svar-ldata
;			     (open-sub-scheme new-pls ii)))
;		      (show ii (length (pls-data red-pls)))
;	      (format t "Open history" (zopen-history  (car (pls-opens current-pls))))
;	      when (pls-data red-pls)
;	      do (setq current-pls (open-sub-scheme new-pls ii))
;	      (format t "~%Taking the ~A open with history " ii)
;			      (return 'done)
;	      else do (format t "~%The ~A open is empty")
;	      finally (return-from sue  blew-up))
;	finally (return (list blew-up i))))
;
;(defun blowup-pls-using-high-dimensional-components (pls n &aux (current-pls pls)
;						     op codim)
;  (loop for i from n  while (pls-data current-pls)
;	do (multiple-value ( op codim)
;	     (high-dimensional-component pls))
;	(format t "~%The ~A blowup: the ~A component of the ~A open of codimension ~A."
;		i (second op) (car op) codim)
;	(setq current-pls (copy-tree (s-blow current-pls (car op) (second op))))))


;;performs the transform of a list of equations into new equations on the
;;ITH blowup of the FIRSTK coordinates  AMBIENT-DIM space
;(defun proper-transform (list-eqns ith firstk ambient-dim &aux
;			 ichart fns subs trans  answ gg)
;  (cond ((eq (car list-eqns) 'ldata)
;	 (setq gg (ldata-inequality list-eqns))
;	 (setq list-eqns (ldata-eqns list-eqns))))
;   (setq ichart (ichart ith firstk ambient-dim))
;   (setq fns (rmap-fns (zopen-inv ichart)))
;   (setq subs (my-pairlis  *xxx* fns))
;   (setq trans (loop for v in list-eqns collecting (psublis subs 1 v)))
;   (cond (gg (setq gg (psublis subs 1 gg))))
;   (setq answ($cancel_factors_and_denominators (cons '(mlist)
;						     trans) (list (xxx ith)) t))
;   (cond (gg (make-ldata :eqns answ :inequality gg))
;	 (t answ)))

(defun proper-transform (ld ith firstk ambient-dim &aux
			 ichart  trans answ  )
   (setq ichart (ichart ith firstk ambient-dim))
   (setq ld (copy-list ld))
   (setq trans  (apply-rmap (zopen-inv ichart) ld))
   (format t "~%Exceptional divisor is ~/maxima::tilde-q-fsh/." (xxx ith))
   (setq answ
	 (cdr (eliminate-common-factors (cons (pexpt (xxx ith) 10) (ldata-eqns trans) ))))


   ;(setq answ($cancel_factors_and_denominators (cons '(mlist)(ldata-eqns trans))
;					       (list (xxx ith))
					       ;;homogeneous cancellation
;					       t))
   (setf (ldata-eqns trans ) answ)
   trans)



;
;(defun blowup-sheaf (pls firstk &key opens-not-to-blowup opens-to-blowup
;		     simplify-homogeneous
;		     throw-out-components-in-exceptional-divisor
;		     (keep-history t)&aux answ tem pt)
;
;  (setq pls (copy-list-structure pls))
;  (let* ((svar (pls-s-var pls))
;	 (data (pls-data pls))
;	 (opens (sv-zopens svar))
;	 dim)
;        (setq dim   (zopen-dim (first (pls-opens pls))))
;    (loop for op in opens
;	  for  dl in data
;	  for ii from 0
;	  when (or (and (null opens-to-blowup) (not (member ii opens-not-to-blowup :test #'eq)))
;		   (mem ii opens-to-blowup :test #'eq))
;	  do (format t "~%Blowing up open number ~A " ii)
;	  and
;	  appending
;	  (loop for i from 1 to firstk
;		do (setq tem (iblowup op i firstk))
;                (setq pt (loop for ldat in dl
;			       collecting
;			       (proper-transform ldat
;						 i firstk dim)))
;		(iassert (eq (car tem) 'zopen))
;		(push (xxx i) (zopen-history tem))
;		when throw-out-components-in-exceptional-divisor
;		do (setq pt (loop for v in pt collecting
;				  (copy-structure
;				    v ldata-
;				    eqns
;				    (loop for f in (ldata-eqns v)
;					  with mon = (pexpt (xxx i) 10)
;							collecting
;							(car (eliminate-common-factors
;								    (list f mon)))))))
;
;		when simplify-homogeneous
;		do (setq pt (loop for v in pt appending (ldata-simplify-homogeneous v
;			       (zopen-inequality tem))))
;		collecting (cons tem pt))
;	  into paired-data
;	  else
;	  collecting (cons op dl) into paired-data
;	  finally  (return(setq answ (make-pre-ldata-sheaves
;				      s-var (make-s-var zopens (mapcar 'car paired-data))
;				      data (mapcar 'cdr paired-data)))))
;    (fsignal "hi")
;    (cond (keep-history (add-pls-zopen-history answ)))
;    answ))

(defun blowup-sheaf (pls firstk &key opens-not-to-blowup opens-to-blowup
		     simplify-homogeneous add-exceptional-divisor-ldata
		     (keep-history t)&aux answ tem pt)
  (setq pls (copy-list-structure pls))

  (let* ((svar (pls-s-var pls))
	 (data (pls-data pls))
	 (opens (sv-zopens svar))
	 dim)
    (setq dim   (zopen-dim (first (pls-opens pls))))
    (loop for op in opens
	  for  dl in data

	  for ii from 0
		  do	  (des dl)
	  when (or (and (null opens-to-blowup) (not (member ii opens-not-to-blowup :test #'eq)))
		   (member ii opens-to-blowup :test #'eq))
	  do (format t "~%Blowing up open number ~A " ii)

	  and
	  appending
	  (loop for i from 1 to firstk
		do (setq tem (iblowup op i firstk))
		(setq pt (loop for ldat in dl
			       do (iassert (ldatap ldat))
			       collecting
			       (proper-transform ldat
						 i firstk dim)))
		(iassert (eq (car tem) 'zopen))
		(push (xxx i) (zopen-history tem))
		when simplify-homogeneous
		do (setq pt (loop for v in pt appending (ldata-simplify-homogeneous v
			       (zopen-inequality tem))))
		when add-exceptional-divisor-ldata
		do (setq pt (append pt (list (make-ldata :eqns (list (xxx i))))))
		collecting (cons tem pt))
	  into paired-data
	  else
	  collecting (progn (cond (add-exceptional-divisor-ldata
				   (setq dl (append dl (list
							  (make-ldata :eqns '(1)))))))
			    (cons op dl)) into paired-data
	  finally (return(setq answ (make-pre-ldata-sheaves
				      :s-var (make-s-var :zopens (mapcar 'car paired-data))
				      :data (mapcar 'cdr paired-data)))))
    (cond (keep-history (add-pls-zopen-history answ)))
    answ))


(defun linear-poly-solve (poly var)
  (ratreduce   (pminus (pcoeff poly 1 (list var))) (pcoeff poly (list var 1 1))))

(defun unused-variables (n &optional strin)
  (cond (strin (setq strin  (concatenate 'string (string strin) "NEW")))
	(t (setq strin "NEW")))
  (loop for i from 1 to n
	collecting (add-newvar (intern (format nil "$~A~A" strin i)))))

;(defun apply-rmap (rmap  fns &key (coords-for-fn *xxx*) subs
;		   &aux (the-denom (rmap-denom rmap)))
;  "Argument may be list of polys, or rat'l fns, result is always rat'l"
;   (cond(subs nil)
;	(t  (setq subs(my-pairlis coords-for-fn (rmap-fns rmap)))))
;  (cond ((polynomialp fns)
;	 (rsublis subs the-denom fns :reduce t))
;	((rational-functionp fns)
;	 (ratquotient   (rsublis subs the-denom (num fns) )
;			(rsublis subs the-denom (denom fns))))
;	((eq (car fns) 'ldata)
;	 (make-ldata eqns (loop for v in (ldata-eqns fns)
;				collecting (function-numerator (apply-rmap rmap v
;							    :coords-for-fn
;							    coords-for-fn
;							    :subs subs)))
;		     inequality (function-numerator(apply-rmap rmap (ldata-inequality fns)
;						    :coords-for-fn
;							    coords-for-fn
;							    :subs subs))))
;	((or (polynomialp (car fns))
;	     (rational-functionp (car fns))
;	     (ldatap (car fns)))
;	 (loop for v in fns
;	       collecting (apply-rmap rmap v :coords-for-fn coords-for-fn
;				      :subs subs)))
;	(t (merror "fns should be a polynomial, rat'l fn,ldata or list of such"))))
;
;(defun apply-rmap (rmap  fns &key (coords-for-fn *xxx*) subs
;		   &aux )
;  "Argument may be list of polys, or rat'l fns, result is always rat'l"
;   (cond(subs nil)
;	(t  (setq subs (subs-for-simple-rat-sublis
;			 coords-for-fn
;			 (loop for f in (rmap-fns rmap)
;			       collecting (ratreduce
;					    f
;					    (rmap-denom rmap)))))))
;  (cond ((or (polynomialp fns)(rational-functionp fns))
;	 (simple-rat-sublis subs fns))
;	((eq (car fns) 'ldata)
;	 (make-ldata eqns (loop for v in (ldata-eqns fns)
;				collecting (function-numerator (apply-rmap rmap v
;							    :coords-for-fn
;							    coords-for-fn
;							    :subs subs)))
;		     inequality (function-numerator(apply-rmap rmap (ldata-inequality fns)
;						    :coords-for-fn
;							    coords-for-fn
;							    :subs subs))))
;	((or (polynomialp (car fns))
;	     (rational-functionp (car fns))
;	     (ldatap (car fns)))
;	 (loop for v in fns
;	       collecting (apply-rmap rmap v :coords-for-fn coords-for-fn
;				      :subs subs)))
;	(t (merror "fns should be a polynomial, rat'l fn,ldata or list of such"))))

(defun apply-rmap (rmap  fns &key (coords-for-fn *xxx*) subs &aux answ)
  "Argument may be list of polys, or rat'l fns, result is always rat'l"
  (new-rmap rmap)
  (cond (subs
	 nil)
	(t
	 (setq subs (subs-for-simple-rat-sublis coords-for-fn (rmap-fns rmap)))))
  (cond ((or (polynomialp fns) (rational-functionp fns))
	 (setq answ (simple-rat-sublis subs fns))
	 (process-sleep 5)
	 answ)
	((eq (car fns) 'ldata)
	 (make-ldata :eqns
		     (loop for v in (ldata-eqns fns)
			collecting (function-numerator (apply-rmap rmap v
								   :coords-for-fn
								   coords-for-fn
								   :subs subs)))
		     :inequality (function-numerator(apply-rmap rmap (ldata-inequality fns)
								:coords-for-fn
								coords-for-fn
								:subs subs))))
	((or (polynomialp (car fns))
	     (rational-functionp (car fns))
	     (ldatap (car fns)))
	 (loop for v in fns
	    collecting (apply-rmap rmap v :coords-for-fn coords-for-fn
				   :subs subs)))
	(t (merror "fns should be a polynomial, rat'l fn,ldata or list of such"))))

(defun ldatap (obj)
  (and (consp obj)(eq (car obj) 'ldata)))

;;;;idea is to take the eqns in ldata and
;;;;construct an rmap to a coordinate system where the
;;;;elements of ldata are transformed to the first n
;;;;eqns where n is the number of eqns in ldata
;;;;we need the ldata since the gg is necessary to figure out which
;;;;variables were linear and should be solved for.
;;(defun normalize-zopen (ldata dim &aux (gg (ldata-inequality ldata))
;;			(eqns (ldata-eqns ldata))
;;			vari  all-eqns all-vari lis map fns
;;			other newvars new-eqns     denom
;;			 n tem)
;;  (setq vari   (loop for v in eqns
;;		     when (setq tem         (any-linearp v gg ))
;;		     do (show tem) and
;;		     collecting tem
;;		     else do (merror "Hey this is not linear ~A" v)))
;;  (setq other (loop for w in *xxx*
;;		    for i below dim
;;		    when (not (member w vari :test #'eq))
;;		    collecting w))
;;  (setq all-vari (append vari other))
;;  (setq newvars (unused-variables dim))
;;  (cond ((not (eql (length all-vari) dim)) (merror "Some variables are wrong in vari")))
;;  (setq new-eqns
;;	(loop for v in eqns
;;	      for u in newvars
;;	      for va in vari
;;	      collecting (linear-poly-solve (pdifference v (list u 1 1)) va)))
;;  (setq fns (append eqns  other))
;;  (setq all-eqns (append new-eqns (loop for u in (nthcdr (length eqns) newvars)
;;					collecting (list u 1 1))))
;; (setq all-eqns  (gen-psublis  (append vari other) newvars   all-eqns))
;;;  (setq all-eqns
;;;	(gen-psublis   other  (nthcdr (length eqns) newvars)
;;;                 all-eqns))
;;  (setq all-eqns
;;	(sublis   (pairlis newvars    *xxx*)
;;                 all-eqns))
;;  (setq lis (make-list dim))
;;   (loop for v in all-vari
;;		       for u in all-eqns
;;		       do (setq n (find-position-in-list v *xxx*))
;;		       (setf (nth n lis) u))
;;   (setq map (construct-rmap lis))
;;   (setq gg (num  (apply-rmap map gg)))
;;   (setq fns (construct-rmap fns))
;;  (make-zopen coord fns inv (construct-rmap lis) inequality  gg))
;
;;;idea is to take the eqns in ldata and
;;;construct an rmap to a coordinate system where the
;;;elements of ldata are transformed to the first n
;;;eqns where n is the number of eqns in ldata
;;;we need the ldata since the gg is necessary to figure out which
;;;variables were linear and should be solved for.
;
;(defun normalize-zopen
;       (ldata dim &aux (gg (ldata-inequality ldata))
;	(eqns (ldata-eqns ldata))
;	vari fns all-new
;	other newvars new-eqns subs rest-new  map denom
;	n tem)
;  (setq vari   (loop for v in eqns
;		     when (setq tem         (any-linearp v gg ))
;		     do (show tem) and
;		     collecting tem
;		     else do (merror "Hey this is not linear ~A" v)))
;  (setq other (loop for w in *xxx*
;		    for i below dim
;		    when (not (member w vari :test #'eq))
;		    collecting w))
;  (setq vari (append vari other))
;  (setq newvars (unused-variables dim))
;  (cond ((not (eql (length vari) dim)) (merror "Some variables are wrong in vari")))
;  (setq new-eqns
;	(loop for v in eqns
;	      for u in newvars
;	      for va in vari
;	      collecting (linear-poly-solve (pdifference v (list u 1 1)) va)))
;  (setq rest-new (loop for i from  (length eqns)  below dim
;		      collecting (nth i newvars)))
;  (setq all-new (append new-eqns rest-new))
;  (setq subs  (subs-for-psublis   other rest-new ))
;  (setq new-eqns
;	(loop for w in new-eqns
;		       collecting (cons (psublis subs 1 (car w))
;					(psublis subs 1 (cdr w)))))
;;   (setq all-subs (append subs (my-pairlis vari new-eqns)))
;  (setq new-eqns  (append new-eqns rest-new))
;;  (make-zopen coord vari inv (append new-eqns rest-new)
;  (setq map  (construct-rmap new-eqns))
;  ;;assumes denom in x1,x2,.. coords
;;  (setq denom  (apply-rmap  map gg))
;  (setq fns (rmap-fns map))
;  (setq new-eqns
;	(loop for v in vari
;	      do (setq n (find-position-in-list v vari))
;	      collecting   (nth n fns)))
;  (show newvars)
;  (show  (subs-for-psublis newvars *xxx*))
;  (setq new-eqns (gen-psublis newvars *xxx* new-eqns))
;  (setf (rmap-fns map) new-eqns)
;  (setf (rmap-denom map) (gen-psublis newvars *xxx* (rmap-denom map)))
;  map)



;  "takes as arguments a list of polys or rat'l fns and tries to make
; these into the first coordinates.  It tries to find the inverse.
; It will check whether it has found the inverse, and error if not
;
; some variable.    It collects these linear variables and then completes
; them to get a list up to the  dimension of the space.  (f1,f2, ..
; fm,xi1,xi2,..) is then the set of coordinates,  and it calculates the
; inverse of this set. It returns the open with inequality gg  and coord
; (f1,...) and inverse These are to become the first nwhich are to be the
; first in the list of  coordinates of some open.  It then completes the
; list of "

(defun make-normal-zopen (eqns dim gg &aux (check-inverse t)
			  variables  fns tem all-variables answ
			  coords newvar coord-eqns solu dif-eqns other-monoms other-variables)
  (setq newvar (unused-variables dim))
  (setq dif-eqns (loop for v in eqns
		    for u in newvar
		    collecting
		      (function-numerator (n- v  (list u 1 1)))))
  (setq variables (linear-ldatap (make-ldata :eqns eqns) :open-g gg))
  (cond ((null variables)
	 (setq variables
	       (loop for v in dif-eqns
		  collecting (setq tem (any-linearp v gg :variables-to-exclude newvar))
		  when (null tem) do
		    (merror "this equation contains no linear " (sh v))))))
  (setq other-variables (loop for v in *xxx*
			   for i below dim
			   when (not (member v variables :test #'eq))
			   collecting v))
  (setq all-variables (append variables other-variables))
  (setq other-monoms
	(loop for v in other-variables
	       collecting (list v 1 1)))

  (setq coord-eqns (loop for v in (setq coords (append eqns other-monoms))
			  for u in newvar
			  collecting (function-numerator (n- v  (list u 1 1)))))
  (setq solu (solve-simple-system (mapcar 'function-numerator coord-eqns) all-variables
				  :invertible gg))
					;  (setq solu  (loop for v in all-variables
					;		    for eqn in coord-eqns
					;		    collecting (linear-poly-solve eqn v)))
					;  (setq solu (gen-psublis other-variables (nthcdr (length eqns) newvar) solu))
  (setq solu      (gen-psublis newvar *xxx* solu))
  (setq fns (make-list dim))
  (setq fns (loop for v in all-variables
		    for f in solu
		    collecting  (nth  (find-position-in-list v *xxx*) solu)))
  (loop for v in all-variables
	 for f in solu
	 do (setf  (nth  (find-position-in-list v *xxx*) fns) f))
  (setq answ (make-zopen :coord
			 (construct-rmap coords) :inv (construct-rmap fns) :inequality gg))
  (cond (check-inverse (check-zopen-inv answ)))
  answ)
;
;(defun make-normal-zopen (eqns dim gg &aux (check-inverse t)
;     variables  fns tem all-variables answ
;				     coords
;				     newvar coord-eqns solu other-monoms other-variables)
;
;  "takes as arguments a list of polys f1,f2,..each of which is linear in
; some variable.    It collects these linear variables and then completes
; them to get a list up to the  dimension of the space.  (f1,f2, ..
; fm,xi1,xi2,..) is then the set of coordinates,  and it calculates the
; inverse of this set. It returns the open with inequality gg  and coord
; (f1,...) and inverse These are to become the first nwhich are to be the
; first in the list of  coordinates of some open.  It then completes the
; list of "
;  (setq variables
;	(loop for v in eqns
;	      collecting (setq tem (any-linearp v gg))
;	      when (null tem) do
;	      (merror "this equation contains no linear " (sh v))))
;  (setq other-variables  (loop for v in *xxx*
;			       for i below dim
;			       when (not (member v variables :test #'eq))
;			       collecting v))
;  (setq all-variables (append variables other-variables))
;  (setq other-monoms
;	(loop for v in other-variables
;	      collecting (list v 1 1)))
;  (setq newvar (unused-variables dim))
;  (setq coord-eqns (loop for v in (setq coords (append eqns other-monoms))
;			 for u in newvar
;			 collecting (pdifference v  (list u 1 1))))
;  (setq solu (solve-simple-system (mapcar 'function-numerator coord-eqns) all-variables
;				  :invertible gg))
;;  (setq solu  (loop for v in all-variables
;;		    for eqn in coord-eqns
;;		    collecting (linear-poly-solve eqn v)))
;;  (setq solu (gen-psublis other-variables (nthcdr (length eqns) newvar) solu))
;  (setq solu      (gen-psublis newvar *xxx* solu))
;  (setq fns (make-list dim))
;  (setq fns  (loop for v in all-variables
;		   for f in solu
;		   collecting  (nth  (find-position-in-list v *xxx*) solu)))
;  (loop for v in all-variables
;	for f in solu
;	do (setf  (nth  (find-position-in-list v *xxx*) fns) f))
;  (setq answ (make-zopen coord
;			 (construct-rmap coords) inv (construct-rmap fns) inequality gg))
;  (cond (check-inverse (check-zopen-inv answ)))
;  answ)


(defun check-zopen-inv (zopen &aux ma)
 (setq ma   (compose-rmap (zopen-inv zopen) (zopen-coord zopen)))
 (loop for v in (rmap-fns ma)
       for u in *xxx*
       when (not (eql (caar v) u))
       do (return (merror "not the inverse")))
 'ok)

(defun solve-simple-system (eqns variables
			    &key (invertible 1) &aux used-up simpl-eqns
			    (all (copy-list eqns)) tem v)
  (setq simpl-eqns
	(loop while all
	      do (setq v (car all))
	      when (setq tem (any-linearp v invertible :among-variables variables))
	      do (setq all (replace-functions all v tem :invertible-g invertible))
	      (setq used-up (cons v (replace-functions
				      used-up v tem :invertible-g invertible)))
	      finally (return (append used-up all))))
  (loop  for v in variables
	 collecting
	 (loop for w in simpl-eqns
	       when (eq (pdegree w v ) 1)
	       ;;(poly-linearp w v invertible) the variables get replaced
	       do (return       (linear-poly-solve w v)))))

(defun normalize-zopen (zopen eqns &key inequality data &aux op answ)
;  (declare (values norm-open data))
  (cond ((null inequality) (setq inequality (zopen-inequality zopen))))
  (setq op (make-normal-zopen eqns (zopen-dim zopen) inequality ))
  (setq answ (copy-list zopen))
  (set-slots answ zopen-  coord (compose-rmap (zopen-coord op) (zopen-coord zopen))
	      inv (compose-rmap (zopen-inv zopen) (zopen-inv op))
	      inequality (function-numerator (apply-rmap (zopen-inv op) inequality)))
  (cond (data  (setq data (apply-rmap (zopen-inv op) data))))
  (values  answ data))


(defun normalize-zopen-in-pls (pls zopen-number eqns
			       &key copy &aux new-open opens open lis-dat MAPL )

  (cond (copy (setq pls (copy-tree pls)))
	(t (setq pls (copy-list-structure pls))))
  (setq opens (pls-opens pls))
  (setq open (nth zopen-number opens))
  (setq lis-dat (nth zopen-number (pls-data pls)))
  (setq new-open (normalize-zopen open eqns))
  (setq MAPL (find-ring-map open new-open))
  (setf (nth zopen-number opens) new-open)
  (setf (nth zopen-number (pls-data pls))
	(apply-rmap MAPL lis-dat))
  pls)
(defun copy-list-structure (expr)
  (cond ((atom expr) expr)
	((member (car expr) '(zopens pre-ldata-sheaves rmap s-var ldata) :test #'eq)
	 (copy-list expr))
	(t (loop for v on expr
		 do (setf (car v) (copy-list-structure (car v)))))))
;
;(defun open-all-refinement ( list-eqns open-g )
;  (loop for v in list-eqns
;	when (setq tem (all-linearp v open-g))
;	collecting v into lin-eqns
;	collecting lin-vars into lin-vars
;	else
;	collecting v into fns
;	collecting (setq tem (gm-all-prepared v :inequal open-g)) into  prep-vars
;	finally
;	(loop for v in prep-vars
;	      for w in fns when (null v)do (fsignal "Not gm-prepared ~A" w))
;
;;(setq vtree (gm-all-prepared (st-rat #$x*y+x^2*w+s*t+1$)))
;((#:S #:W #:Y . OK) (#:S #:Y #:W . OK)
;                    (#:T #:W #:Y . OK)
;                    (#:T #:Y #:W . OK)
;                    (#:W #:S #:Y . OK)
;                    (#:W #:T #:Y . OK)
;                    (#:W #:Y #:S . OK)
;                    (#:W #:Y #:T . OK)
;                    (#:Y #:S #:W . OK)
;                    (#:Y #:T #:W . OK)
;                    (#:Y #:W #:S . OK)
;                    (#:Y #:W #:T . OK))
;;now check refinements wrt to different ones for lower complexity
;;will get for each f a list of lists of cofs that need inverting to make
;;covering.  We can factor them and use eliminate-larger to find the best
;;cove;


;;should return an open cover such that the functions are all linear
;;they should start out well prepared.  You  shortest such covering
;;Note some functions depend on the fact that the first
;;element returned by best-open-cover is the function itself and
(defun best-open-cover (list-fns open-g &aux   tem inv-list prep-fns)
  (cond ((all-linearp list-fns open-g)
	 (list open-g))
	(t (setq prep-fns (loop for v in list-fns
				when (setq tem (gm-prepared v :inequal open-g))
				collecting v ))
	   (cond ((eq (length prep-fns) 1)
		  (best-open-cover1 prep-fns open-g))
		 (t (loop for v in prep-fns
			  do
			  (show v)
			  (setq tem ( best-open-cover1 (list v) open-g))
			  (setq inv-list
				(loop for gg in tem
				      appending
				      (best-open-cover (delete v (copy-list prep-fns) :test #'equal)
							(sftimes open-g gg))))
			  (show inv-list)
			  (setq inv-list (eliminate-multiples inv-list))
			  collecting inv-list into possible
			  minimize (length inv-list) into min
			  do (show min)

			  finally
			  (return
				    (loop for v in possible
					  when (eq (length v ) min)
					  do (return v)))))))))

(defun eliminate-multiples (list-fns &key (square-free t) &aux tem facts)
  (setq facts (loop for v in list-fns
	collecting (setq tem (non-constant-factors v))
	when square-free
	do (loop for v on tem by #'cddr do (setf (second v) 1))))
  (mapcar #'multiply-out-factorization (eliminate-larger facts :test 'a-factor)))

;;this works for a list of one function..
(defun best-open-cover1 (list-fns open-g &aux tem compl-tem  the-prep-fns answ possible-refs lis-prep-cofs lis-prep-var)
  (loop for f in list-fns
     when (any-linearp f open-g)
     collecting f into lins
     else collecting f into prep-fns
     finally (setq the-prep-fns prep-fns)
       (setq lis-prep-var
	     (loop for f in prep-fns
		    collecting (nsubst nil 'ok (gm-all-prepared f :inequal open-g))))

       (setq lis-prep-cofs
	     (loop  for f in prep-fns
		     for lis in lis-prep-var
		     collecting
		     (loop for varl in lis
			    collecting
			    (loop for v in varl
				   collecting
				   (setq tem (non-constant-factors (pcoeff f (list v 1 1 ))))
				   do (loop for v on tem by #'cddr
					     do (setf (second v) 1)))))))
  (setq possible-refs (all-perms lis-prep-cofs))
  (loop for v in possible-refs
	 collecting  (setq tem (eliminate-larger v :test 'a-factor)) into facts
	 minimize (length tem) into min
	 finally
	 (loop for v in facts
		for i from 0
		when (eql (length v) min)
		collecting(setq compl-tem (cons i  (gen-pcomplexity tem))) into compl
		minimize (cdr compl-tem) into min-compl
		finally (loop for v in compl-tem
			       when (eq (cdr compl-tem) min-compl)
			       ;;return the factors for refinement
			       do (return (setq answ(nth (car compl-tem) facts))))))
  (loop for w in  (append the-prep-fns (mapcar 'multiply-out-factorization answ))
	 collecting (nplcm open-g w)))

(defun all-perms (list-lists)
 (cond ((eq (length list-lists) 1) (car list-lists))
  (t (loop for v in (car list-lists)
	   appending
	   (loop for w in (all-perms (cdr list-lists))
		 collecting
		 (append v w))))))

(defun open-refinement (zopen gmprep-poly m  &aux  mzopens next-open cofs)
  "Will return m+1 opens covering the zopen. It is gm-prepared wrt the
 inequality g of zopen."
  (let* ((gg (zopen-inequality zopen))
	 (vari (gm-prepared gmprep-poly :m m :inequal gg)))
    (check-arg zopen (eq (car zopen) 'zopen) "a zopen")
    (check-arg gmprep-poly polynomialp "poly")
    (setq cofs (loop for v in vari
		  collecting (pcoeff gmprep-poly (list v 1 1))))
    (setq mzopens
	  (loop
	     for cof in cofs
	     collecting
	       (zl-copy-structure zopen
				  zopen- inequality (nplcm cof gg)
				  coord (zopen-coord zopen)
				  inv (zopen-inv zopen))))
    (setq next-open

	  (zl-copy-structure zopen zopen- inequality  (nplcm gg
							     gmprep-poly)))
    (cons next-open mzopens)))

;(defun ldata-refinement (ldata gmprep-poly m &key (inequality 1)
;			 &aux all-ldata  cofs)
;  "Will return m ldata covering the zopen. It is gm-prepared wrt the
; inequality g of zopen."
;  (let* ((gg (nplcm inequality (ldata-inequality ldata)))
;;	 (m (length (any-gm-prepared gmprep-poly gg)))
;	 (vari (gm-prepared gmprep-poly :m m :inequal gg)))
;    (check-arg gmprep-poly polynomialp "poly")
;    (setq cofs (loop for v in vari
;		       collecting (pcoeff gmprep-poly (list v 1 1))))
;   (setq all-ldata
;	  (loop
;	    for cof in cofs
;	    collecting
;	    (copy-structure  ldata
;		      ldata- inequality (nplcm cof gg))))
;   all-ldata))

(defun ldata-refinement (ldata gmprep-poly m &key (inequality 1)
			 &aux all-ldata  cofs)
  "Will return m ldata covering the ldata. The open-gs can
 be used as the open number for the simplification."
;  (declare (values all-data open-gs))
  (let* (;;(gg (nplcm inequality (ldata-inequality ldata)))
;	 (m (length (any-gm-prepared gmprep-poly gg)))
	 (vari (gm-prepared gmprep-poly :m m :inequal inequality)))
    (check-arg gmprep-poly polynomialp "poly")
    (setq cofs (loop for v in vari
		       collecting (pcoeff gmprep-poly (list v 1 1))))
    (eliminate-multiples cofs)
    (setq cofs (mapcar 'square-free cofs))
   (setq all-ldata
	  (loop
	    for cof in cofs
	    collecting
	    (zl-copy-structure  ldata
		      ldata- inequality (nplcm cof (ldata-inequality ldata)))))
   (values all-ldata cofs)))

;(defun ldata-refinement (ldata gmprep-poly m &key (inequality 1)
;			 &aux all-ldata  cofs)
;  "Will return m ldata covering the ldata. The open-gs can
; be used as the open number for the simplification."
;  (declare (values all-data open-gs))
;    (check-arg gmprep-poly polynomialp "poly")
;    (setq cofs (best-open-cover gmprep-poly inequality))
;    (iassert (may-invertp gmprep-poly (car cofs)))
;    (setq cofs (cdr cofs))
;   (setq all-ldata
;	  (loop
;	    for cof in cofs
;	    collecting
;	    (copy-structure  ldata
;		      ldata- inequality (sftimes cof (ldata-inequality ldata)))))
;   (values  all-ldata cofs))
;;; the following would have been empty:
;    (setq next-ld
;		 (copy-structure ldata ldata- inequality  (nplcm gg
;							     gmprep-poly)))
;    (cons next-ld all-ldata)))

;;want s-var-ldata-simplifications to take
;;a pre-ldata-sheaves and simplify it.
;; this may involve adding more opens if we meet m-prepared's in
;;the ldata, it will return a pre-ldata-sheaves

;;notes add an argument to (ldata-simplifications ldata gg)
;;to allow passing the gg from the current open.  The
;;computation of the mpreprared should be wrt that gg.
;;still have to modify the ldata-simplifications to return
;; in *stop-simplify* the mprepared fn and m. (and to stop work )

(defun simplify-svar-ldata (pls &key keep-opens-with-empty-ldata
			    (check-containment t)
			    (set-ldata-inequalities-to-one t)
			    (refine-opens t)
			    (keep-history t) opens-not-to-simplify  &aux simped-ld w refs
			    ;;to make the gm-prepared
			    (*refine-opens* refine-opens)
			    some-ld gg a-list answ)
  (setq pls (copy-list-structure pls))
  (loop for i in opens-not-to-simplify
     collecting (nth i (pls-opens pls)) into tem
     finally (setq opens-not-to-simplify tem))
  (let ((svar (pls-s-var pls))
	(data (pls-data pls))
	(*inside-simplify-svar-ldata* t))
    (setq a-list (loop for op in (sv-zopens svar )
		    for dl in data
		    collecting (cons op dl)))
    (loop for v on a-list
       for ii from 0
       do
	 (setq w (car v))
	 (format t "~%Open Number : ~A"(find-position-in-list (car w) (pls-opens pls)))
	 (setq gg (zopen-inequality (car w)))
	 (setq *stop-simplify* nil)
       when (not (member (car w) opens-not-to-simplify :test #'equal))
       do
	 (loop for ld on (cdr w)
		do (setq some-ld (LDATA-SIMPLIFICATIONS (car ld) :open-g gg))
					;		(des (car w))
		appending some-ld into tem
		when *stop-simplify*
		do
		(show *stop-simplify*)

		;;set v so that (cdr v) will be the rest of
		;;the pairs (zopen . (list ldata1 ldata2..))
		(setq refs       (open-refinement (car w) (first *stop-simplify*)
						  (second *stop-simplify*)))
		(format t "~%Refining open number ~D into ~D opens with inequalities ~/maxima::tilde-q-fsh/"
			(find-position-in-list (car w) (pls-opens pls))
			(length refs) (loop for v in refs collecting
					     (zopen-inequality v)))
		(iassert (not (null *refine-opens*)))
		(loop for vv in tem do (check-arg vv (eq (car vv)'ldata) "an ldata"))
		(setq v (cons nil
			      (append
			       (loop for open in refs
				      collecting (cons  open
							(copy-tree (append tem
									   (cdr ld)))))
			       (cdr v))))
		(return 'stopped)
		finally (setq simped-ld
			      (delete-redundant-ldata  tem :ignore-ldata-inequalities t
						       :gg (zopen-inequality
							    (car w)))))
	 (show (length v))
       else do (setq simped-ld (cdr w))
       when (and (null *stop-simplify*) (or (member (car w) opens-not-to-simplify :test #'equal)
					    keep-opens-with-empty-ldata simped-ld))
       do
	 (cond (simped-ld
		(setq simped-ld (delete-redundant-ldata
				 simped-ld :gg (zopen-inequality (car w))))

		(cond (check-containment
		       (check-component-containment  (cdr w)
						     simped-ld
						     (zopen-inequality (car w)))))))


       and
       collecting
	 (car w) into opens
       and
       collecting simped-ld into ldata-list
       finally
	 (return (setq answ (make-pre-ldata-sheaves
				  :s-var  (make-s-var :zopens opens)
				  :data ldata-list)))))
  (cond (set-ldata-inequalities-to-one
	 (set-inequalities-to-one answ) answ))
  (cond (keep-history (add-pls-zopen-history answ)))
  answ)


(defmacro desn (x)
  `(let (*give-coordinates*)
     (des ,x)))
;
;(defun set-inequalities-to-one (lis)
;  (cond ((atom lis) lis)
;	((eq (car lis) 'ldata)		 (setf (ldata-inequality lis) 1))
;	(t
;	 (loop for v on lis while (not (atom v))
;
;	       do (set-inequalities-to-one v)))))
;
;(defun set-inequalities-to-one (lis)
;  (cond ((listp lis)
;	 (cond  ((and (listp  lis)(eq (car lis) 'ldata))
;		 (setf (ldata-inequality lis) 1))
;		(t
;		 (loop for v in lis
;		       do
;		       (set-inequalities-to-one v)))))))
(defun set-inequalities-to-one (form)
  (cond ((null form) nil)
	((atom form ) form)
	((eq (car form ) 'ldata)
			 (setf (ldata-inequality form) 1)
			 form)
	(t (do ((r form (cdr r)))
	       ((not (consp r)) form)
	     (setf (car r) (set-inequalities-to-one (car r))))
	   form)))

(defvar *signal-component-error* nil)

(defun check-component-containment (orig-ldata-list list-ldat &optional (open-g 1) &aux bad-components
				  answ unit   (use-ldata-inverse t) gg)
  (cond ((null open-g) (setq open-g 1)))
  (cond ((ldatap orig-ldata-list)(setq orig-ldata-list (list orig-ldata-list))))
  (loop for v in list-ldat
	for i from 0
	do
	(loop for ld in orig-ldata-list
	      when use-ldata-inverse
	      do(setq gg (plcm open-g (ldata-inequality v)))
	      else do  (setq gg open-g)
	      do
	      (multiple-value-setq
		(answ unit)(grobner-subset (ldata-eqns ld) (ldata-eqns v) gg))
	      when unit do (format t "~%**The component is   empty***")
	      when (or unit answ)
	      do
	      (format t "~%    Component ~D verified:" i) (des v)	      (return 'ok)


	      finally (push i bad-components)
	      (format t  "**The component ~/maxima::tilde-q-fsh/ ~%does not contain the original data***** " v)
	      (cond (*signal-component-error* (fsignal "not contained" v)))))
  bad-components)

(defun check-components-contain-original (ldata list-ldat &aux bad-components answ)
  (loop for v in list-ldat
	for i from 0
	do
     (setq answ (catch 'took-too-long (grobner-remember (ldata-eqns v))))

	(loop for u in (ldata-eqns ldata)
	      when (not ($zerop (polysimp u)))
	      do (mshow u (polysimp u))

	      (format t "**The ideal: ~/maxima::tilde-q-fsh/ ~%does not contain the original one *** : ~/maxima::tilde-q-fsh/"
		      v ldata)
	      (push-new i bad-components)
	      (return (break t)))
	(format t "~%    Component ~D verified:" i)
	(des v))
  (cond (bad-components (format t "~%All but components ~A contained the originals." bad-components)))
  bad-components)

(defun variable-doesnt-occur (var &rest lists-fns)
  (loop for v in lists-fns
	when (member var (list-variables v :test #'eq))
	do (return nil)
	finally (return t)))

;(defun simplify-svar-ldata (pls  &aux simped-dat-list some-ld gg ref the-partial-ldat answ)
;  (let ((svar (pls-s-var pls))
;	(data (pls-data pls))
;	( *inside-simplify-svar-ldata* t))
;    (loop for op on (sv-zopens svar)
;	  for dat-list on data
;	  do
;	  (show op)
;          (show  dat-list)
;	  (setq gg (zopen-inequality (car op)))
;		  (show gg)
;	  (setq *stop-simplify* nil)
;	  (loop for ld in (car dat-list)
;		do (setq some-ld (ldata-simplifications  ld gg ))
;		when (null *stop-simplify*)
;		appending some-ld into tem
;		else
;		do
;		(setq ref (apply  'refinement
;				    (cons (car op) *stop-simplify*)))
;		(setq op (cons nil (append ref (cdr op))))
;		(setq the-partial-ldat (append tem some-ld (cdr ld)))
;		(setq dat-list (cons nil (append
;					   (make-list (length ref)
;						      :initial-value
;						      the-partial-ldat)
;					   (cdr dat-list))))
;		(show dat-list)
;		(return 'stopped)
;		finally (setq simped-dat-list tem)(show simped-dat-list))
;	  ;;otherwise the open will be picked up next trip through the loop
;	  when (null *stop-simplify*)
;	  collecting (car op) into ops
;	  and
;	  collecting simped-dat-list into dats
;	  finally (return (setq answ (make-pre-ldata-sheaves :s-var
;							     (make-s-var zopens ops)
;							     data dats)))))
;  answ)

;;the following is for converting back things which have been dumped to file.
;;or after we have reset the genvar
(defun replace-rmaps-by-new-ones (form)
  (cond ((null form) nil)
	((atom form ) form)
	((eq (car form ) 'rmap)
		     (convert-rmap-to-new form))
	(t (do ((r form (cdr r)))
	       ((not (consp r)) form)
	     (setf (car r) (replace-rmaps-by-new-ones (car r)))))))

(defun rerat (form)
  (cond ((null form) nil)
	((and (symbolp form)
	      (not (member form '(ldata s-var zopens rmap zopen pre-ldata-sheaves
				  quote inequality eqns) :test #'eq)))
	 (add-newvar (intern (string-append "$" (string form)))))
	(t (do ((r form (cdr r)))
	       ((not (consp r)) form)
	     (setf (car r) (rerat (car r)))))))


;(setq pl
;(construct-pre-ldata-sheaves :s-var
;			     (make-s-var :zopens (list (affine-open (firstn 2 *xxx*) )))
;			     :data (list (list
;					   (make-ldata :eqns
;							 (loop for v in '(1 2)
;							       collecting (nth v (grobner-monomials #$[x1,x2]$ 3)))
;
;						       inequality 1)))))
;
;
;(setq test
;      (construct-pre-ldata-sheaves
;	:s-var
;	(make-s-var zopens (list (affine-open (firstn 8 *xxx*) #$1$)))
;	:data (list (list
;		      (make-ldata eqns (st-rat
;					 #$[x1*x2+1+x3*x4,x3^2+x1^2]$)
;				  inequality 1)))))

(defun affine-svar (&key dim  eqns (inequality 1) (ld-inequality 1) list-ldata)
  (cond ((null list-ldata)
	 (setq list-ldata (list (make-ldata :eqns eqns :inequality ld-inequality)))))
      (construct-pre-ldata-sheaves
	:s-var
	(make-s-var :zopens (list (affine-open (subseq *xxx* 0 dim) inequality)))
	:data (list list-ldata)))

(defun affine-ldata (n eqns inequality)
      (construct-pre-ldata-sheaves
	:s-var
	(make-s-var :zopens (list (affine-open (subseq *xxx* 0 n) inequality)))
	:data (list (list
		      (make-ldata :eqns  eqns
				  :inequality inequality)))))



(defmacro for-editor (&body body)
  `(let ((linel 75)
	$display2d)
    ,@ body))


(defmacro desn-editor (expr &aux me)
  (setq me `(setq ,expr (rerat (quote ,(symbol-value expr)))))
  (for-editor (desn (symbol-value expr)))
   (format t "~A" me)
   (values ))

(defun change-strings-to-symbols (tree)
  (cond ((stringp tree) (make-symbol tree))
	((atom tree) tree)
	(t
	 (loop for v on tree
	       do (setf (car v) (change-strings-to-symbols (car v)))
	       finally (return tree)))))


(defun des-file (expr file-name)
  (with-open-file  (st file-name :direction :output)
      (let ((linel 75)(*standard-output* st)
	    $display2d)
	(des expr ))))


;(defmacro des-editor (expr &optional slash &aux me)
;  (setq me `(setq ,expr (rerat (quote ,(eval expr)))))
;  (time:print-current-time)
;  (for-editor (des (symbol-value expr)))
;  (cond (slash
;	 (format t "~%~s" me))
;	(t (format t "~%~a" me)))
;  (values ))


(defun sh-comp (com)
  (loop for v in (car com )do (format t "~%~A" v)))

;(defun des-editor (expr)
;  (for-editor (format t "~A" expr)))
;(defun find-map (open1 open2 &aux answ)
;  "produces a map so that g so that g(coord open1) = (coord open2)"
;  (let ((subs (subs-for-psublis *xxx* (rmap-fns  (zopen-inv open1))))
;	(coord2 (zopen-coord open2)))
;    (setq answ (loop for v in (rmap-fns coord2)
;		     collecting (psublis subs 1 v) into tem
;		     finally (return (make-rmap fns tem denom
;						(psublis subs 1 (rmap-denom coord2))))))
;    (setq answ (reduce-rational-map answ))))

(defun find-ring-map (from-open to-open)
  (compose-rmap (zopen-coord from-open) (zopen-inv to-open)))



(defun translate-component (from-open to-open ldata &key pls &aux MAPL  )
  (cond (pls
	 (setq to-open (nth to-open (pls-opens pls)))
	 (setq ldata (nth ldata (nth from-open (pls-data pls))))
	 (setq from-open (nth from-open (pls-opens pls)))))
  (show (car ldata))
  (cond ((equal from-open to-open) ldata)
	(t
	 (setq MAPL (find-ring-map from-open to-open))
	 (show (length (ldata-eqns ldata)))
	 (apply-rmap MAPL ldata))))

;(defun translate-component-and-reduce (from-open to-open ldata
;				       &aux *refine-opens* hh gg red-transl transl map )
;	 (setq map (find-ring-map from-open to-open))
;	 (setq transl (apply-rmap map ldata))
;	 (setq red-transl (ldata-simplifications transl
;						 (setq gg (zopen-inequality to-open))))
;	 (show red-transl)
;	 (setq hh (nplcm (rmap-denom map) gg))
;	 (setq hh (nplcm (function-numerator (apply-rmap map (zopen-inequality from-open)))
;			hh))
;
;	 (loop for v in red-transl
;	       when (not (unit-idealp (ldata-eqns v) hh))
;	       collecting v into tem
;	       finally (return  (cond ((> (length tem) 1)
;			      (format t
;				      "~%The image had two components meet the intersection.")
;			      (des tem) (break 'two))
;			      ((= (length tem) 1) (car tem))
;			      (t nil)))))

(defun sftimes (f g)
  (square-free (ptimes f g)))


;;;works
(defun apply-rmap-to-square-free-factors (mapl pol &aux answ)
  (let ((facts (non-constant-factors pol)))
    (cond ((null facts) (cons pol 1))
	  (t
	   (loop named sue
	      for v in facts by #'cddr
	      collecting v into tem
	      finally (setq answ (apply-rmap mapl tem))
		(loop for v in answ
		   with answer = (cons 1 1)
		   do (setq answer (rattimes answer v t))
		   finally (return-from sue answer)))))))

;;;did not need to compute the translated ldata except on the intersection
(defun translate-component-and-reduce (from-open to-open ldata &key (use-inverse-inequal t)
				       &aux *refine-opens* hh gg red-transl answ
				       inv inv-denom transl MAPL )
;  (declare (values ldata intersection-inequality-on-to-open))
  (setq MAPL (find-ring-map from-open to-open))
  (process-sleep 20)
  (setq gg (zopen-inequality to-open))
  (cond (use-inverse-inequal
	 (setq inv (find-ring-map to-open from-open))
	 (setq inv-denom (function-numerator (apply-rmap-to-square-free-factors
				      MAPL (rmap-denom inv))))
	 (setq hh (sftimes gg inv-denom))
	 (process-sleep 20))
	(t (setq hh gg)))

  (setq transl (apply-rmap MAPL ldata))
  (setq hh (sftimes (rmap-denom MAPL) hh))
  (setq hh (sftimes (function-numerator (apply-rmap MAPL (zopen-inequality from-open)))
		  hh))
  ;;calculate on open intersection (possibly using the inv-denom !!)
  (setq red-transl (ldata-simplifications transl :open-g hh))
  (show red-transl)
  (setq answ
	(loop for v in red-transl
	      when (not (unit-idealp (ldata-eqns v) hh))
	      collecting v into tem
	      finally (return  (cond ((> (length tem) 1)
				      (format t
					      "~%The image had two components meet the intersection.")
				      (des tem) (fsignal  'two-components))
				     ((= (length tem) 1) (car tem))
				     (t nil)))))
  (values  answ hh))

;(defun translate-reduced-component-and-reduce (from-open to-open ldata &aux leng)
;  (multiple-value-bind ( answ hh)
;      (translate-component-and-reduce from-open to-open ldata
;				      :use-inverse-inequal nil)
;    (show answ)
;    (cond ((null answ) (values answ hh))
;	  ((equal leng (length (ldata-eqns ldata)))
;	   (cond ((null use-inverse-inequal)
;
;	   (translate-component-and-reduce from-open to-open ldata
;					   :use-inverse-inequal t
;					   ))))
;	  (t (values  answ hh)))))

(defun ldata-codim (ldata)
  (length (ldata-eqns ldata)))
;;to hell with the expense: use translate the correct intersection inequality.
(defun translate-reduced-component-and-reduce (from-open to-open ldata
					       &key homogeneous-ldata-on-to-open
					       &aux answer int-transl
						contract simp-contract)
  (multiple-value-bind ( answ hh)
      (translate-component-and-reduce from-open to-open ldata
				      :use-inverse-inequal t)
    (cond ((null answ)(values answ hh))
	  (t
	   (setq
	     answer
	     (cond ((or
		      (linear-solvedp (ldata-eqns answ))
		      (linear-ldatap answ :open-g (zopen-inequality to-open)))
		    answ)
		   (t (setq contract
			    (contract-ideal-localization (ldata-eqns answ)
							 hh))
		      (mshow contract)
		      (setq simp-contract (simplify-ldata (make-ldata :eqns contract)
							  :open-g
							  (zopen-inequality to-open)))
		      (cond ((null simp-contract) (values simp-contract hh))
			    ((> (length simp-contract) 2)
			     (loop for v in simp-contract
				   when (and  (null (unit-idealp (ldata-eqns v) hh))
					      (equal (ldata-codim v)
						     (ldata-codim ldata)))
				   collecting v into some-ld
				   finally (cond ((> (length some-ld) 1)
						  (fsignal "Too many components"))
						 ((eq (length some-ld) 1)
						  (car some-ld))
						 (t nil))))
			    (t (setq simp-contract (car simp-contract)))))))
	   (cond   ((not (equal (length (ldata-eqns answer))
				(length (ldata-eqns ldata))))
		    (cond ((null *query-user*)
			   (throw 'new-choice nil))
			  (homogeneous-ldata-on-to-open
			   (setq int-transl
				 (intersect-ldata homogeneous-ldata-on-to-open
						  answ :open-g hh))
			   (fsignal "Wrong dimension or not complete intersection ~
			      look at the value of int-transl "))
			  (t (setq answer answ)))

		    ))))
    (cond ((and *query-user* answer
		(loop for v in (ldata-eqns answer)
		      when (not (or (any-linearp v (zopen-inequality to-open))
				    (gm-prepared v :inequal (zopen-inequality to-open))))
		      do (return 'bad)))
	   (fsignal "not linear or well prepared on to open")))
  (values answer hh)))

;(defun translate-reduced-component (from-open to-open ldata pls
;				    &key( break-bad-component t)
;				    &aux map  gg hh answer
;				    image answ cont unit *stop-simplify*)
;  "Takes eqns (which one might want to turn into coordinates) and translates them
; around to the other opens.  For example if one had an ldata of some one open
; and wanted to blow it up one would need the translates of this, before blowing up."
;
;
;  (cond ((eq from-open to-open) ldata)
;	(t
;	 (setq to-open (nth to-open (pls-opens pls)))
;	 (setq from-open (nth from-open (pls-opens pls)))
;	 (setq hh (zopen-inequality to-open ))
;	 (setq gg (zopen-inequality from-open ))
;	 (cond ((ldatap ldata) nil)
;	       (t (setq ldata (make-ldata :eqns ldata :inequality
;					  1 ))))
;	 (setq map (find-ring-map from-open to-open))
;	 (setq image       (apply-rmap map ldata))
;	 (setq gg (function-numerator (apply-rmap map gg)))
;	 (setq hh (nplcm hh gg))
;	 (setq hh (nplcm (rmap-denom map) hh))
;	 (setq answ (ldata-simplifications image))
;	 (loop for v in answ
;	       do (multiple-value
;		    (cont unit)
;		    (grobner-subset (ldata-eqns v)
;				    (ldata-eqns image)
;				    hh))
;	       (cond (unit (format t "%intersection is empty") (return 'empty))
;		     (cont (return (setq answer v))))
;	       finally (cond (answ (return
;				     (cond (break-bad-component
;					    (merror "bad-comp")
;					    (break 'bad-component))
;					   (t (format t "****Bad component: not trivial****")))))))
;	 answer)))



(defun ldata-subset (ld ldd)
	       (grobner-subset (ldata-eqns ld) (ldata-eqns ldd) (ldata-inequality ldd)))

;(defun test (pl n m l &aux opens im (ldata (nth n (nth m (pls-data pl)))))
;  (setq opens (pls-opens pl))
;  (setq im (translate-component (nth m opens) (nth l opens) ldata))
;;  (setq denom (apply-rmap ma (rmap-denom (coord
;  (loop for ld in (nth l (pls-data pl))
;	when (grobner-subset ld im)
;	do (format t "image is:") (shl im)
;		   (format t" containing")(des ld)
;  (des (nth  l (pls-data pl)))))


(defun pls-ldata (pls OPEN &key ldata-number &aux answer)
  "Gets the list of ldata corresponding to OPEN"
  (cond ((numberp open)(setq open (nth open (pls-opens pls)))))
  (check-arg open (eq (car open) 'zopen) "an open")
  (setq answer (loop for v in (pls-opens  pls)
	for w in (pls-data pls)
	when (equal  open v)
	do (return w)))
  (cond (ldata-number (nth ldata-number answer))
	(t answer)))

;(defun function-numerator (f)
;  (cond ((polynomialp f) f)
;	((rational-functionp f) (num f))
;	(t (num (new-rat f)))))
;
;(defun match-components (pls nth-open &aux map opens open gg image cont unit answ)
;  (setq opens (pls-opens pls))
;  (setq open (nth nth-open opens))
;  (setq answ
;	(loop for ld in  (pls-ldata pls open)
;	      for ii from 0
;	      collecting
;	      (loop for op in opens
;		    for i from 0
;		    for lis-ld in (pls-data pls)
;		    when (not (eql i nth-open))
;		    collecting
;		    (progn
;		      (setq map    (find-ring-map open  op))
;		      (setq image (apply-rmap  map ld))
;		      (setq gg (function-numerator (apply-rmap map (zopen-inequality open))))
;		      (setq gg (plcm (rmap-denom map) gg))
;		      (loop for ldd in lis-ld
;			    for j from 0
;			    do
;			    (multiple-value (cont unit)
;			      (grobner-subset (ldata-eqns ldd)
;					      (ldata-eqns image)
;					      (plcm (zopen-inequality op) gg)))
;
;			    (cond (unit
;				   (format
;				     t
;				     "~%The intersection with the ~D open is empty."
;				     i)))
;			    (cond ((and (null unit) cont)
;				   (cond ((null (grobner-subset
;						  (ldata-eqns image)
;						  (ldata-eqns ldd)
;						  (plcm (zopen-inequality op) gg)))
;
;					  (merror "containment only one way!!")))))
;			    when unit do (return nil)
;			    when (and (null unit)cont)
;			    collecting (cons i j) into tem
;			    finally
;			    (cond ((and lis-ld (null tem))
;				   (merror "this component not trivial")))
;			    (return tem))))))
;  (loop for u in answ
;	do (des (car u))
;	(loop for vv in (cdr u)
;	      do
;	      (loop for v in vv
;		    do
;		    (format t
;			    "~%on open ~A the ~D component is contained in its image"
;				    (car v)(cdr v)))))
;  answ)


(defun union-equal (&rest lists &aux result)
  (loop for l in lists do
       (loop for w in l do
	    (pushnew w result :test #'equal)))
  (nreverse result))

(defun intersection-equal1 (&rest l)
  (cond ((eq (length l) 1) (car l))
	(t (apply #'intersection-equal (loop for v in (car l)
					  when (member v (second l) :test #'equal)
					  collecting v)
		  (cddr l)))))

(defun intersectp (a b &key test)
  (loop named sue for v in a
	do (loop for w in b
		 when (funcall test v w)
		 do (return-from sue t))))


;;;doesn 't work!! eg if str1 is nil
;(defun build-equivalence-relation1 (str1 str2 &aux str3)
;  (setq str3
;	(loop for v in str1
;	collecting
;	(loop for w in str2
;	      when (intersectp v w :test 'equal)
;	      appending (union-equal v w) into tem))))

(defun build-equiv (lis &key (test #'equal))
  (block sue
    (loop for v in lis
       do (loop for w in lis
	     when (and (not (eql v w)) (intersectp v w :test test))
	     do
	       (setq lis (cons (union-equal v w)
			       (delete w (delete v lis :test #'equal) :test #'equal)))
	       (return-from sue (build-equiv lis :test test)))
       finally (return (mapcar #'union-equal lis)))))

(defun check-equivalence-relation (lis-equiv-classes)
  (block sue
    (loop for v in lis-equiv-classes
       do (loop for w in lis-equiv-classes
	     when (intersectp v  w :test 'equal)
	     do (cond ((not (eql v w)) (return-from sue nil))))
       finally (return t))))

(defun add-zopen-history (zopen n)
  (cond ((>= (length zopen) 5)
	 (zl-copy-structure zopen zopen- history (cons n (zopen-history zopen))))
	(t (setq zopen (nconc zopen (copy-list (list nil) )))
	   (add-zopen-history zopen n))))

(defvar *bad-components* nil)
;
;(defun new-match-components (pls open-number)
;  (declare (special str))
;  (loop for ld in (pls-ldata open-number)
;	for ii from 0
;	do
;	(new-match-component pls open-number ii)))
;
;(defun new-match-component (pls open ld-number &aux (opens (pls-opens pls))  hh nth-open
;			    ok map image gg simped images cont unit *bad-components* ld)
;  (setq nth-open (find-position-in-list opens))
;  (setq ld  (pls-ldata 0 :ldata-number ld-number))
;  (loop for op in (pls-opens pls)
;	for i from 0
;	for lis-ld in (pls-data pls)
;	when  (equal op open)
;	do (push (cons nth-open ld-number ) str)
;	else
;	do (setq map (find-ring-map op open))
;	(setq image (apply-rmap map ld))
;	(setq gg (function-numerator (apply-rmap map (zopen-inequality open))))
;	(setq gg (plcm (rmap-denom map) gg))
;	(setq images (list image))
;	(setq ok nil)
;	(setq simped nil)
;	(loop  while images
;	      do (setq image (car  images))
;	      (setq images (cdr images))
;	      (loop for an-ld in lis-ld
;		    for j from 0
;		    do
;		    (multiple-value (cont unit)
;		      (grobner-subset (ldata-eqns an-ld)
;				      (ldata-eqns image)
;				      (setq hh   (plcm (zopen-inequality op) gg))))
;		    when cont
;		    do (setq ok t) (push (cons nth-open  j) str)
;		    when unit
;		    do (push (cons nth-open nil) str)
;		    (setq ok t) (return 'empty))
;	      finally (cond ((and lis-ld (null ok))
;			     (cond ((null simped)
;				    (setq images (ldata-simplifications image))
;				    (setq simped t)
;				    (cond ((equal (ldata-eqns (car images))
;						  (ldata-eqns  image))
;					   (setq images nil))))
;				   (t
;
;				    (push (list nth-open  i ld-number) *bad-components*)
;				    (format t "~%Bad component ~A" (car *bad-components*)))))))))

(defun final-check-contained-in (lis-ld orig-image hh current-open
				 current-ld-number nth-open
				 &aux images cont unit *refine-opens*)
  (setq images (ldata-simplifications orig-image :open-g hh))
  (loop named sue for image in images
	do
	(loop for v in lis-ld
	      for i from 0
	      do
	      (multiple-value (cont unit)
		(grobner-subset (ldata-eqns v) (ldata-eqns image) hh))
	      when unit
	      do (return 'unit)
	      when
	      cont do (cond ((eq (length (ldata-eqns v))
				 (length (ldata-eqns image)))
			     (return-from sue  (cons current-open i)))
			    (t (format t "image properly contains zl-SOME part"))))

	finally (push (list nth-open current-open
			    current-ld-number) *bad-components*)
	(format t "~%Bad component ~A" (car *bad-components*))
	(break t)))
;
;(defun match-components (pls nth-open &key check-equal break-on-bad ignore-empty-opens
;			 ldata-number &aux map opens open gg hh image cont unit answ found-one str lis-dat rev-cont)
;  (setq opens (pls-opens pls))
;  (setq open (nth nth-open opens))
; (setq lis-dat (pls-ldata pls open) )
; (cond (ldata-number (setq lis-dat (list (nth ldata-number lis-dat)))))
;  (setq answ
;	(loop for ld in lis-dat
;	      for ii from 0
;	      ;;str will be ( (open number . ld number) ...)
;	      when  ldata-number
;	      do (setq str (list (cons nth-open ldata-number)))
;	      else
;	      do
;	      (setq str (list  (cons nth-open ii)))
;	      do
;	      (loop for op in opens
;		    for i from 0
;		    for lis-ld in (pls-data pls)
;		    when (and (not (eql i nth-open)) (or lis-ld (null ignore-empty-opens)))
;		    do (setq found-one nil)
;		    (progn
;		      (setq map    (find-ring-map open  op))
;		      (setq image (apply-rmap  map ld))
;		      (setq gg (function-numerator (apply-rmap map (zopen-inequality open))))
;		      (setq gg (nplcm (rmap-denom map) gg))
;		      (loop for ldd in lis-ld
;			    for j from 0
;			    do
;			    (multiple-value (cont unit)
;			      (grobner-subset (ldata-eqns ldd)
;					      (ldata-eqns image)
;					  (setq hh   (nplcm (zopen-inequality op) gg))))
;			    (cond ((and cont check-equal (null unit))
;				   (setq rev-cont (grobner-subset (ldata-eqns ldd)
;								  (ldata-eqns image)
;								  hh))
;				   (cond ((null rev-cont)
;					  (cond (break-on-bad (break 'not-equal)))))))
;
;			    (cond (unit
;				   (format
;				     t
;				     "~%The intersection with the ~D open is empty."
;				     i)))
;;			    (cond ((and (null unit) cont)
;;				   (cond ((null (grobner-subset
;;						  (ldata-eqns image)
;;						  (ldata-eqns ldd)
;;						  (nplcm (zopen-inequality op) gg)))
;;
;;					  (merror "containment only one way!!")))))
;			    when unit do (push (cons i nil) str)(setq found-one t)
;			    when (and (null unit)cont)
;                             do (push (cons i j) str) (setq found-one t)
;			    finally
;			    (cond ((and lis-ld (null found-one))
;				   (push  (list nth-open ii i open ld) *bad-components*)
;				   (format t "~%*****Bad component..****")
;				   (format t " origninal open and component are:")
;				   (des open) (des ld)
;				   (format t "%On the image open the components are:")
;				   (loop for l in lis-ld do (des l))
;				   (format t "while image is :")
;				   (des image)
;				   (cond (break-on-bad (break t))))
;				  ))))
;;				   (merror
;	;			     "this component not trivial but doesn't seem to be here"))))))
;	      collecting str))
;  answ)



(defun match-components (pls nth-open &key check-equal break-on-bad ignore-empty-opens
			 ldata-number &aux MAPL opens open gg hh tem
			 image cont unit answ found-one str lis-dat rev-cont)
  (setq opens (pls-opens pls))
  (setq open (nth nth-open opens))
 (setq lis-dat (pls-ldata pls open) )
 (cond (ldata-number (setq lis-dat (list (nth ldata-number lis-dat)))))
  (setq answ
	(loop for ld in lis-dat
	      for ii from 0
	      ;;str will be ( (open number . ld number) ...)
	      when  ldata-number
	      do (setq str (list (cons nth-open ldata-number)))
	      else
	      do
	      (setq str (list  (cons nth-open ii)))
	      do
	      (loop for op in opens
		    for i from 0
		    for lis-ld in (pls-data pls)
		    when (and (not (eql i nth-open)) (or lis-ld (null ignore-empty-opens)))
		    do (setq found-one nil)
		    (progn
		      (setq MAPL    (find-ring-map open  op))
		      (setq image (apply-rmap  MAPL ld))
		      (setq gg (function-numerator (apply-rmap MAPL (zopen-inequality open))))
		      (setq gg (nplcm (rmap-denom MAPL) gg))
		      (loop for ldd in lis-ld
			    for j from 0
			    do
			    (multiple-value (cont unit)
			      (grobner-subset (ldata-eqns ldd)
					      (ldata-eqns image)
					  (setq hh   (nplcm (zopen-inequality op) gg))))
			    (cond ((and cont check-equal (null unit))
				   (setq rev-cont (grobner-subset (ldata-eqns ldd)
								  (ldata-eqns image)
								  hh))
				   (cond ((null rev-cont)
					  (cond (break-on-bad (break 'not-equal)))))))

			    (cond (unit
				   (format
				     t
				     "~%The intersection with the ~D open is empty."
				     i)))
;			    (cond ((and (null unit) cont)
;				   (cond ((null (grobner-subset
;						  (ldata-eqns image)
;						  (ldata-eqns ldd)
;						  (nplcm (zopen-inequality op) gg)))
;
;					  (merror "containment only one way!!")))))
			    when unit do (push (cons i nil) str)(setq found-one t)
			    when (and (null unit)cont)
			     do (push (cons i j) str) (setq found-one t)
			    finally
			    (cond ((and lis-ld (null found-one))
				   (setq tem  (final-check-contained-in
				     lis-ld image hh i j nth-open))
				   (cond (tem (push tem str))))
;				   (push  (list nth-open ii i open ld) *bad-components*)
;				   (format t "~%*****Bad component..****")
;				   (format t " origninal open and component are:")
;				   (des open) (des ld)
;				   (format t "%On the image open the components are:")
;				   (loop for l in lis-ld do (des l))
;				   (format t "while image is :")
;				   (des image)
;				   (cond (break-on-bad (break t))))
				  ))))
;				   (merror
	;			     "this component not trivial but doesn't seem to be here"))))))
	      collecting str))
  answ)





(defun list-opens1 (components &aux answ)
  (loop for com in components
     do (loop for v in com do
	     (push-new (car v) answ))
     finally (return answ)))

(defun short-list-open-numbers ( components-to-cover &optional opens-used
			       &aux where an-answ ops)
  (cond ((null components-to-cover ) opens-used)
	(t (setq ops(list-opens1 components-to-cover))
	   (loop for op in ops
		 with prev-min = 10000000
		 do
		 (setq an-answ
		 (loop for com in components-to-cover
		       when (not (assoc op com :test #'eq))
		       collecting com into tem
		       finally (return (short-list-open-numbers tem
							       (cons op opens-used)))))
		 when (< (length an-answ) prev-min)
		 do (setq prev-min (length an-answ)) (setq where an-answ)
		 finally (return where)))))



(defun fast-match-components (pls &aux components-to-cover transl hh to-open to-lis-dat
			      components all-codims ops bad-one)
  "we assume that the pls has been reduced"
  (loop for op in (pls-opens pls)
	for op-num from 0
	for lis-dat in (pls-data  pls)
	do
	(setq components-to-cover nil)
	(loop for ld in lis-dat
	      for i from 0
	      when (not (or (linear-solvedp (ldata-eqns ld))
			    (linear-ldatap ld)))
	      do (setq bad-one ld)
	      (setq components-to-cover components) (return 'done)
	      else
	      do
	      (loop for dim in all-codims
		    for comp in components
		    when (equal dim (length (ldata-eqns ld)))
		    do (push-new comp components-to-cover)))
	(setq ops (short-list-open-numbers components-to-cover ))
	(setq ops (ml-sort ops ))
		(show op-num ops)
	(loop for to-op-num in ops
		      with accounted-for
	      do  (setq to-open   (nth to-op-num (pls-opens pls)))
	      (setq   to-lis-dat  (nth to-op-num (pls-data pls)))
	      (loop for lld in lis-dat
		    for lld-num from 0
		    when (not (member lld accounted-for :test #'eq))
		    do
		    (multiple-value-setq
		      (transl hh) (translate-reduced-component-and-reduce
				    op to-open lld))
		    when transl
		    do
		    (loop for ld in to-lis-dat
			  for ld-num from 0
			  when
			  (and (not (unit-idealp (ldata-eqns ld) hh))
			  (variety-ldata-subset ld transl :open-g hh
						      :ignore-ldata-inequalities t))
			  do
			  (cond
			    ((variety-ldata-subset
			       transl ld :open-g hh
			       :ignore-ldata-inequalities t)
			     (nconc (get-component components (cons to-op-num ld-num))
				    (list (cons op-num lld-num)))
			     (push lld accounted-for))
			    (t (fsignal "containment one way")))))
	      finally (loop for lld in lis-dat
			    for lld-num from 0
			    when (not (member lld accounted-for :test #'eq))
			    do (show to-op-num lld-num)
			    and
			    collecting (list (cons op-num  lld-num)) into comps
			    and
			    collecting (length (ldata-eqns lld)) into dims
			    finally (setq components (nconc components comps))
			    (setq all-codims (append all-codims dims)))))
 (values components all-codims))

(defun get-component (components cons-op-ld)
  (loop for com in components
	when (member cons-op-ld com :test #'equal)
	do (return com)))
(defun list-opens-with-component (pls open-num compon-num &aux answ)
  (setq answ (nth compon-num (match-components pls open-num)))
  (des (nth compon-num (nth open-num (pls-data pls))))
  (format t "~%is in opens: ")
  (loop for op in (pls-opens pls)
	for i from 0
	when  (eq (cdr (assoc i answ :test #'equal)) compon-num)
	collecting i))

(defun match-all-components (pls &key (ignore-empty-opens t)
			     &aux all equiv opens *bad-components*)

  (setq opens (pls-opens pls))
  (setq all (loop for i below (length opens)
	appending (match-components pls i :ignore-empty-opens ignore-empty-opens)))
  (setq equiv (build-equiv all :test #'(lambda (u v)
					 (and (equal u v)  (cdr v)))))

;  (cond ((check-equivalence-relation equiv) equiv)
;	(t (merror "some components don't match up ~A" equiv)))
  (setq equiv (loop for v in equiv
		    collecting (sort v #'(lambda (u v) (< (car u) (car v))))))
	       (list 'components  equiv *bad-components*))

(defun verify-simplification (pl1 pl2simp)
  (loop for open in (sv-zopens (pls-s-var pl1))
	for i from 0
	for lis-dat in (pls-data pl1)
	do
	(format t "~%**For the original ~:R open" i)
	(loop  for op2 in (sv-zopens (pls-s-var pl2simp))
	       for j from 0
	       for lis-dat2 in (pls-data pl2simp)
	       when (equal (zopen-coord open) (zopen-coord op2))
	       ;; should really look at inequal	       when (equal  open  op2)
	       do (format t " whose coordinates are equal to those on open ~D"  j)
	       (cond (lis-dat2
		      (check-component-containment  lis-dat lis-dat2))
;	       (check-components-contain-original (car lis-dat) lis-dat2))
		     (t (format t "the data was empty"))))))


;;the following system of equations does not admit a nice solution using the above.
;;maybe we have to add another divide-dichotomy machine:
;;If have two polynomials with leading term x6 could do a dichotomy:
;;f=y^i*a+.. g=y^j*b+.. then do
;; while y is the main variable  you make dichotomy
;;between (third (vdivide f g))=0 and (third (vdivide f g)) invertible.
;;in the latter system you have the additional ldata of remainder.

;;(LDATA ((X7 1 1) (X8 1 1) (X5 2 1 1 (X4 2 -2 0 (X3 1 (X2 1 1))) 0 (X4 4 1 2 (X3 1 (X2 1 1)) 1 (X3 1 (X1 1 2)))) (X6 1 (X5 1 (X2 1 1) 0 (X4 2 (X2 1 1) 1 (X1 1 2))) 0 (X5 1 (X4 1 (X2 1 3) 0 (X1 1 2)) 0 (X4 3 (X2 1 -1)))) (X6 1 (X5 1 1 0 (X4 2 -1)) 0 (X5 1 (X4 1 -1) 0 (X4 3 1 1 (X3 1 (X2 1 -1)) 0 (X3 1 (X1 1 -1))))) (X5 1 (X2 2 1) 0 (X1 2 -1))) 1 2)
(defmacro minimize ( for v in a-list quantity)
;  (declare (values where-its-miniminum))
  (iassert (and (eq for 'for) in 'in))
  `(loop for ,v in ,a-list
	 with .prev-min. = 10000000
	 with .where.
	 do (setq .tem. ,quantity)
	 when (< .tem. .prev-min.)
	 do (setq .where. ,v)
	 (setq .prev-min. .tem.)
	 finally (return .where.)))

(defmacro for (v in-or-on a-list &rest body &aux when-clause operation quantity pred op init)
  "operation may be :minimize, maximize, general-summing, or general-product;
   and the the quantity will ususually involve the variable v.  The result of the
   minimize is where the minimum is"
  (iassert (member in-or-on '(in on) :test #'eq))
  (cond ((equal (car body) 'when)
	 (setq when-clause (subseq body 0 2) body (cddr body)))
	(t (iassert (eq (length body) 2))))
  (setq operation (first body) quantity (second body))
  (cond ((member operation '(minimize maximize) :test #'equal)
	 (cond ((eq operation 'minimize)
		(setq pred	  '<)
		(setq init 10000000))
	       (t (setq pred '>) (setq init -100000000)))
	 `(loop for ,v ,in-or-on ,a-list
		with .prev-min. = ,init
		with .where.
		,@ when-clause
		do
		(setq .tem. ,quantity)
		,@ (cond (when-clause '(and))(t nil))
		when (,pred .tem. .prev-min.)
		do (setq .where. ,v)
		(setq .prev-min. .tem.)
		finally (return .where.)))
	((member operation '(general-summing general-product) :test #'equal)
	 (cond ((eq operation 'general-summing)
		(setq op 'N+) (setq init 0))
	       ((eq operation 'general-product)
		(setq op 'n*) (setq init 1)))
	 `(loop for ,v ,in-or-on ,a-list
		with .answer. = ,init
		,@ when-clause
		do (setq .answer. (,op ,quantity  .answer.))
		finally (return  .answer.)))
	(t (fsignal "The operation was not one of minimize, maximize, general-summing, or general-product"))))



;;(for v in '(-1 -3 -.5 2 3) when (< v 0) minimize (abs  v ))

;;(for v in '(-1 $x -3 -.5 2 3)  general-product   v )
;;(for v in '(-1 $x -3 -.5 2 3) when (> v 0)  general-product   v )

;
;(defun good-order-variables (eqns &aux variable-occurs (varl (list-variables eqns)))
;  (declare (special mult))
;  (declare (values . (list ordered-list variable-occurs-in)))
;  (setq variable-occurs (mapcar 'list-variables eqns))
;  (setq mult
;	(loop for v in
;	varl
;	collecting
;	(cons v
;	(loop for w in variable-occurs
;	      for f in eqns
;	      when (member v w :test #'eq)
;	      count 1    ))))
;  (setq mult  (sort mult #'(lambda (u v)
;			    (< (cdr u) (cdr v)))))
;  (list   (mapcar 'car mult) variable-occurs))


;;this allowed me to do the troublesome ldata
;;it tries to order the variables so that the variables belonging to
;;simpler and less equations come first.
;(defun good-order-variables (eqns &aux variable-occurs (varl (list-variables eqns))
;			     compl)
;  (declare (special mult))
;  (declare (values . (list ordered-list variable-occurs-in)))
;  (setq variable-occurs (mapcar 'list-variables eqns))
;  (setq compl  (loop for v in eqns collecting (gen-pcomplexity v)))
;  (setq mult
;	(loop for v in
;	varl
;	collecting
;	(cons v
;	(loop for w in variable-occurs
;	      for f in eqns
;	      when (member v w :test #'eq)
;	      count 1 into the-mult
;	      finally (return (* the-mult (loop for com in compl
;						for ww in variable-occurs
;						when (member v ww :test #'eq)
;						minimize com)))))))
;  (setq mult  (sort mult #'(lambda (u v)
;			    (< (cdr u) (cdr v)))))
;  (list   (mapcar 'car mult) variable-occurs))

(defun good-order-variables (ldata &aux variable-occurs varl order eqns
			     compl)
  (declare (special mult))
					;  (declare (values . (list ordered-list variable-occurs-in)))
  (cond ((ldatap ldata)
	 (cond ((ldata-variables ldata)(list (ldata-variables ldata) (mapcar 'list-variables
									     (ldata-eqns ldata))))
	       (t (setq order (good-order-variables (ldata-eqns ldata)))
		  (setf (ldata-variables ldata) (car order))
		  order)))
	(t (setq eqns ldata)
	   (setq varl (list-variables eqns))
	   (setq variable-occurs (mapcar 'list-variables eqns))
	   (setq compl (loop for v in eqns collecting (gen-pcomplexity v)))
	   (setq mult
		 (loop for v in varl
		    collecting
		      (cons v
			    (loop for w in variable-occurs
			       for f in eqns
			       when (member v w :test #'eq)
			       count 1 into the-mult
			       finally (return (* the-mult (loop for com in compl
								   for ww in variable-occurs
								   when (member v ww :test #'eq)
								   minimize com)))))))
	   (setq mult  (sort mult #'(lambda (u v)
				      (< (cdr u) (cdr v)))))
	   (list   (mapcar 'car mult) variable-occurs))))


(defun charactaristic-setp (eqns &aux lis answ ordered-vars highest-vars occurs )
  (setq lis (good-order-variables eqns))
  (setq ordered-vars (first lis))
  (setq highest-vars
	(loop for v in (setq  occurs (second lis))
	collecting (loop for u in ordered-vars
			 when (member u v :test #'eq)
			 do (return u))))
  (setq answ (loop for v on highest-vars
	when (member (car v) (cdr v) :test #'eq)
	do (return nil)
	finally (return t)))
  (values answ ordered-vars occurs highest-vars))



;(defun order-equations (eqns variables &optional occurs &aux all-eqns)
;  (cond ((null occurs) (setq occurs (mapcar 'list-variables eqns))))
;  (loop for va in variables
;	appending
;    (loop for eqn in eqns
;	  for oc in occurs
;	  when (member va oc :test #'eq)
;	  do (push-new eqn all-eqns)))
; (nreverse all-eqns))

(defun order-equations (eqns &aux tem)
;  (declare     (values eqns ch-set))
  (multiple-value-bind (ch-set vars ignore highest-vars)
      (charactaristic-setp eqns)
    (setq eqns   (cond (ch-set
			(loop for v in vars
			      when (setq tem (find-position-in-list v highest-vars))
			      collecting (nth tem eqns)))
		       (t eqns)))
    (values eqns ch-set)))

(defun highest-variables (variables occurs)
  (loop for oc in occurs
	collecting (loop for v in variables
			 when (member v oc :test #'eq) do (return v))))

(defun all-linear-variables (f &optional g &aux varl)
 (setq varl (degree-one-variables f))
 (loop for v in varl when (may-invertp (pcoeff f (list v  1 1 ) ) g)
	collecting v))

(defun poly-relations-from-simplifications (&optional (simps *poly-simplifications*))
  (loop for (seq repl) on simps by #'cddr
	when (numberp seq) do (return '( 1))
	collecting
	(pdifference
	  (ptimes (convert-deg-sequence-to-monomial seq)
		  (denom repl))
	  (num repl))))

(defun simplify-ldata (ldata &key (open-g 1) refine-opens error-check-containments
		       &aux answ result changed)
  (let ((*refine-opens* refine-opens) *stop-simplify*)
    (setq answ (ldata-simplifications ldata :open-g open-g :error-check-containments
				      error-check-containments))

    (setq answ (loop for ld in answ
		     when (linear-ldatap ld :open-g open-g)
		     collecting ld
		     else
		     do (loop for fn in (ldata-eqns ld)
			      when (> (length (non-constant-factors fn)) 2)
			      do(setq changed t)
			      (return (setq result (make-dichotomy ld :open-g open-g)))
			      finally (setq result (list ld)))
		     appending result))
    (cond (changed (setq answ (delete-redundant-ldata  answ :gg open-g))))
    answ))

;;this is hoky. It should try much harder!!
(defvar *dont-try-factor-irreducible-ldata* nil)

(defun try-factor-irreducible-ldata (ldata &optional (open-g 1) &aux answ orig-ldata answer
				     lin-vars eqn varl comp new-ldata tem  (eqns (ldata-eqns ldata)))
  (declare (special *already-tried*))
  (cond ((not (boundp '*already-tried*)) (setq *already-tried* nil)))
  (setq orig-ldata ldata)
  ;;should be wrt open-g
  (cond ((member(setq tem (list ldata open-g)) *already-tried* :test #'equal)
	 (list ldata))
	(*dont-try-factor-irreducible-ldata* (list ldata))
	(t
	 (setq answer
	       (catch 'took-too-long
		 (push tem *already-tried*)
		 (cond
		   ((setq varl (linear-ldatap ldata :open-g open-g))
		    (setq eqns(reduce-linear-ldata ldata varl :open-g open-g))
		    (cond ((grobner-subset '(1) eqns open-g) nil)
			  (t (list (zl-copy-structure ldata ldata- eqns eqns)))))
		   ((< (setq comp (gen-pcomplexity (ldata-eqns ldata))) 200)
		    (LET (*poly-simplifications*)
		      (grobner-remember (ldata-eqns ldata))
		      (cond (*poly-simplifications*
			     (setq new-ldata (make-ldata :eqns (poly-relations-from-simplifications)
							 :inequality (plcm open-g (ldata-inequality ldata)))))))
		    (cond ((< (gen-pcomplexity (ldata-eqns new-ldata)) (* 1.1 comp))
			   (setq ldata new-ldata)))

		    (setq lin-vars (loop for f in eqns collecting (all-linear-variables f open-g)))
		    (show lin-vars)
		    (setq answ
			  (block sue
			    (loop for v in lin-vars
			       for f in eqns
			       when (null v)
			       do (setq varl (list-variables f))
				 (loop for w in lin-vars
				    for ff in eqns
				    when (setq tem (intersection w varl))
				    do
				      (loop for uu in tem
					 do (setq eqn (gen-prem f ff uu))
					 when (> (length  (non-constant-factors eqn open-g)) 2)
					 do (return-from sue (make-dichotomy
							      (zl-copy-structure ldata ldata- eqns (subst eqn f eqns))
							      :open-g  open-g))))
			       finally (return (list ldata)))))
		    (loop for ld in answ
		       minimize (length (ldata-eqns ld)) into min
		       finally (cond ((> min (length (ldata-eqns orig-ldata)))
				      (setq answ (list orig-ldata)))
				     (t nil)))
		    answ)
		   (t (list ldata)))))
	 (cond ((eq answer 'took-too-long)
		(list ldata))
	       (t answer)))))

(defun matrix-to-sparse-matrix (mat &key (re-use-sparse-matrix *sparse-matrix*))
  (convert-to-sparse-matrix (matrix-rows mat) :re-use-sparse-matrix re-use-sparse-matrix
			    ))

(defun matrix-rank (mat &aux sp)
 (setq sp  (matrix-to-sparse-matrix mat))
  (sp-reduce sp)
  (sp-number-of-pivots sp))


(defun show-matrix (mat)
  (cond ((arrayp mat)
  (let* ((dimensions (array-dimensions mat))
	(number-dims (length dimensions)))
    (cond ((eq number-dims 2)
	   (loop for i below (car dimensions)
		 do (format t "~%")
		 (loop for j below (second dimensions)
		       do (format t "~3D" (aref mat i j)))))
	  ((eq number-dims 3)
	   (loop for i below (car dimensions)
		 do (format t "~%~%Block ~D " i)
		 (loop for j below (second dimensions)
		       do (format t "~%")
		       (loop for k below (third dimensions)
			     do (format t "~3D" (aref mat i j k)))))))))
	((matrix-p mat)
	 (loop for v in (matrix-rows mat)
	       do (format t "~% ~A " v)))))

(defun jacobian-matrix (list-eqns &optional variables &aux mat)
  (cond ((null variables)(setq variables (ml-sort (list-variables list-eqns)))))
  (setq mat (loop for f in list-eqns
	collecting (loop
		     for v in variables
		     collecting (pderivative f v))))
  (make-matrix :rows mat))



;(defun divide-dichotomy (ldata &key (open-g 1) &aux (eqns (ldata-eqns ldata)) f new-eqns
;			 occurs vars highest-vars orig-rep repeat eqns-rep  gg ld1 ld2)
;
;  "endeavors to turn ldata into an  triangular list of eqns
; so that each equation has  possibly one more variable occurring than the
; previous.  It takes a good order for the variables and then takes the first variable
; to be highest in two succeeding eqns, and does a division to try to correct this.  If
; the leading variable is not invertible it does a dichotomy.   This dichotomy must be
; at the level of open sets, since otherwise we will not get component containment."
;  ;;ordering should take into account open-g
;  (setq vars  (good-order-variables eqns))
;  (setq occurs (second vars))
;  (setq vars (first vars))
;  (setq highest-vars
;	(loop for v in occurs
;	      collecting (loop for u in vars
;			       when (member u v :test #'eq)
;			       do (return u))))
;  (show vars highest-vars)
;  (setq repeat
;	(loop named rep for v in vars
;	      do
;	      (loop for w on highest-vars
;		    when (and (eq (car w) v)
;			      (member v (cdr w) :test #'eq))
;		    do (return-from rep v))))
;  (cond
;    (repeat
;     (setq eqns-rep  (loop for v in eqns
;			   for u in highest-vars
;			   when (eq u repeat)
;			   collecting v))
;     (setq orig-rep (copy-list eqns-rep))
;     (show (length eqns-rep))
;     (setq eqns-rep (sort-key eqns-rep '< 'pdegree repeat))
;;     (progn (declare (special repeat))
;;     (setq eqns-rep (sort eqns-rep
;;			  #'(lambda (u v) (< (pdegree u repeat) (pdegree v repeat))))
;     (show (length eqns-rep))
;
;     (cond ((eq ( pdegree (first eqns-rep) repeat)
;		(pdegree (second eqns-rep) repeat))
;	    ;;choose the least complex leading coefficient to divide by
;	    (setq eqns-rep
;		  (sort-key (firstn 2 eqns-rep) '<
;			    #'(lambda (u var) (pcomplexity
;						(leading-coefficient u var)))
;			    repeat))))
;     (setq f (second eqns-rep))
;
;     (show repeat)
;
;     (multiple-value-bind (rem c-reqd)
;	 (gen-prem  f (first eqns-rep) repeat)
;
;       (shl (list f  (first eqns-rep)))
;       (setq new-eqns (delete f
;			      (copy-list eqns)))
;       (cond (($zerop rem) nil)
;	     (t  (setq new-eqns  ;;these have f replaced by rem
;		       (append new-eqns (list rem)))))
;;       (setq gg (nplcm open-g (ldata-inequality ldata)))
;;       (cond ((may-invertp c-reqd  gg)
;;	      (setq ld2 (make-ldata eqns  new-eqns
;;				    inequality  gg )))
;;	     ;;make two ldata one where c-reqd is invertible  and f replaced by rem
;;	     ;; and other where its zero and f is still there.
;;	     (t (setq ld2 (make-ldata eqns new-eqns
;;				      inequality (nplcm gg c-reqd)))
;;		(setq ld1 (make-ldata  eqns
;;				       (cons c-reqd eqns)
;;				       inequality gg))))
;;       (cond (ld1
;;	      (format t "~%Breaking into dichotomy on:")
;;	      (sh c-reqd)
;;	      (format t "%original ldata followed by consequents:" )
;;	      (des ldata)(des ld1) (des ld2 )
;;
;;	      (append (ldata-simplifications ld1)
;;		      (ldata-simplifications ld2)))
;;	     (t
;;	      (format t "~%The c-reqd was invertible: ")
;;	      (sh c-reqd)
;;	      (ldata-simplifications ld2)))))
;;    (t (list ldata))))
;
;
;;;I hope this was the one to keep
;(defun divide-dichotomy (ldata &key (open-g 1) &aux answ (eqns (ldata-eqns ldata)) f new-eqns
;			 occurs vars highest-vars orig-rep repeat eqns-rep  gg ld1 ld2)
;
;  "endeavors to turn ldata into an  triangular list of eqns
; so that each equation has  possibly one more variable occurring than the
; previous.  It takes a good order for the variables and then takes the first variable
; to be highest in two succeeding eqns, and does a division to try to correct this.  If
; the leading variable is not invertible it does a dichotomy.   This dichotomy must be
; at the level of open sets, since otherwise we will not get component containment."
;  ;;ordering should take into account open-g
;  (setq vars  (good-order-variables eqns))
;  (setq occurs (second vars))
;  (setq vars (first vars))
;  (setq highest-vars
;	(loop for v in occurs
;	      collecting (loop for u in vars
;			       when (member u v :test #'eq)
;			       do (return u))))
;  (show vars highest-vars)
;  (setq repeat
;	(loop named rep for v in vars
;	      do
;	      (loop for w on highest-vars
;		    when (and (eq (car w) v)
;			      (member v (cdr w) :test #'eq))
;		    do (return-from rep v))))
;  (cond
;    (repeat
;     (setq eqns-rep  (loop for v in eqns
;			   for u in highest-vars
;			   when (eq u repeat)
;			   collecting v))
;     (setq orig-rep (copy-list eqns-rep))
;     (show (length eqns-rep))
;     (setq eqns-rep (sort-key eqns-rep '< 'pdegree repeat))
;;     (progn (declare (special repeat))
;;     (setq eqns-rep (sort eqns-rep
;;			  #'(lambda (u v) (< (pdegree u repeat) (pdegree v repeat))))
;     (show (length eqns-rep))
;
;     (cond ((eq ( pdegree (first eqns-rep) repeat)
;		(pdegree (second eqns-rep) repeat))
;	    ;;choose the least complex leading coefficient to divide by
;	    (setq eqns-rep
;		  (sort-key (firstn 2 eqns-rep) '<
;			    #'(lambda (u var) (pcomplexity
;						(leading-coefficient u var)))
;			    repeat))))
;     (setq f (second eqns-rep))
;     (cond ((not (equal       (ml-sort (copy-list eqns-rep))
;			      (ml-sort (copy-list  orig-rep))))
;	    (merror "not sorted but destroyed!!")))
;     (show repeat)
;
;     (multiple-value-bind (rem c-reqd)
;	 (gen-prem  f (first eqns-rep) repeat)
;       (shl (list f  (first eqns-rep)))
;       (setq new-eqns (delete f
;			      (copy-list eqns)))
;       (cond (($zerop rem) nil)
;	     (t  (setq new-eqns  ;;these have f replaced by rem
;		       (append new-eqns (list rem)))))
;       (setq gg (nplcm open-g (ldata-inequality ldata)))
;       (mshow open-g)
;       (cond ((may-invertp c-reqd  gg)
;	      (list (ldata-simplifications  (make-ldata eqns  new-eqns
;							inequality  gg ) open-g)))
;	     (t
;	      (setq ld1 (make-ldata eqns (cons f new-eqns) inequality (plcm c-reqd gg)))
;	      (setq ld2  (make-ldata eqns (cons c-reqd (ldata-eqns ldata)) inequality gg))
;	      (check-component-containment ldata (list ld1 ld2) open-g)
;	      (setq answ(append
;		(ldata-simplifications ld1 open-g)
;		(ldata-simplifications   ld2    open-g)))
;	      (check-component-containment ldata answ open-g)
;	      answ))))
;    (t (list ldata))))


;;from version 7  ;;this worked on orig so be careful about modifying it!!!.
;;watch out for the  use-inverse in delete redundant  switch.

(defvar *used-divisors* nil)

(defun find-repeats (highest-variables eqns &aux first-repeat repeat-eqns)
  (setq first-repeat
	(loop for v on highest-variables
	       for eqnss on eqns
	       when  (member (car v) (cdr v) :test #'eq)
	       do (cond ((cddr (setq repeat-eqns
				     (loop for eqn in eqnss
					    for va in v
					    when (and (eq va v)
						      (not (member eqn *used-divisors* :test #'equal)))
					    collecting v)))
			 (return (car v))))))
  (cond ((null (cddr repeat-eqns)) (setq repeat-eqns nil)))
  (values  repeat-eqns first-repeat))

;
;(defun new-divide-dichotomy (ldata &key (open-g 1) &aux var-and-occurs variables eqns occurs highest-variables
;			 divisor ld1 ld2
;			 repeated-eqns high-var)
;  (setq var-and-occurs (good-order-variables ldata))
;  (setq variables (car var-and-occurs) occurs (second var-and-occurs))
;  (setq eqns (order-equations (ldata-eqns ldata) variables occurs))
;  (setq occurs (mapcar 'list-variables eqns))
;  (setq highest-variables (highest-variables variables occurs))
;  (multiple-value-setq (repeated-eqns high-var)
;		       (find-repeats highest-variables eqns))
;  (cond
;    (repeated-eqns
;     (setq repeated-eqns (sort-key repeated-eqns '< 'pdegree high-var))
;
;     (setq divisor
;	   (loop for v in repeated-eqns
;		 find v minimizing (+ (* 1000 (pdegree v high-var))
;				      (pcomplexity v))))
;     (multiple-value-bind (rem c-reqd)
;	 (gen-prem (second repeated-eqns) divisor high-var)
;       (setq ld1 (copy-list-structure ldata))
;       (setf (ldata-eqns ld1)
;	     (cons rem (delete (second repeated-eqns) (copy-list eqns))))
;       (iassert (not (member (second repeated-eqns) (ldata-eqns ld1))))
;       (mshow divisor (second repeated-eqns) rem c-reqd)
;       (cond ((may-invertp c-reqd open-g)
;	      (format t "  which was invertible")
;	      (ldata-simplifications
;		ld1 :open-g open-g))
;	     (t
;	      (setq ld2 (copy-list-structure ldata))
;	      (setf (ldata-eqns ld2) (cons c-reqd (ldata-eqns ldata)))
;	      (setf (ldata-inequality ld1) (nplcm (ldata-inequality ldata)
;						  c-reqd))
;	      (des ld1)(des ld2)
;	      (append
;		(ldata-simplifications
;		  ld1 :open-g open-g)
;		(ldata-simplifications
;		  ld2 :open-g open-g))))))
;    (t (list ldata))))







(defun  divide-dichotomy (ldata &key (open-g 1) &aux answer  used
			  (eqns (ldata-eqns ldata)) f new-eqns
			  occurs vars highest-vars orig-rep repeat eqns-rep  gg ld1 ld2)

  "endeavors to turn ldata into an  triangular list of eqns
 so that each equation has  possibly one more variable occurring than the
 previous.  It takes a good order for the variables and then takes the first variable
 to be highest in two succeeding eqns, and does a division to try to correct this.  If
 the leading variable is not invertible it does a dichotomy "

  ;;ordering should take into account open-g

  (setq ldata (copy-list-structure ldata))
  (setq vars (copy-tree  (good-order-variables eqns)))
  (setq occurs (second vars))
  (setq vars (first vars))
  (setq highest-vars
	(loop for v in occurs
	      collecting (loop for u in vars
			       when (member u v :test #'eq)
			       do (return u))))
  (show vars highest-vars)

  (multiple-value-setq (eqns-rep repeat)
    (find-repeats vars eqns))
  (unwind-protect
    (progn
      (cond
	(repeat
	 (setq orig-rep (copy-list eqns-rep))
	 (show (length eqns-rep))
	 (setq eqns-rep (sort-key eqns-rep '< 'pdegree repeat))
	 (setq used  (cons repeat (pdegree (first eqns-rep) repeat)))
;         (push repeat *used-divisors*)
	 (show (length eqns-rep))
	 (cond ((eq ( pdegree (first eqns-rep) repeat)
		    (pdegree (second eqns-rep) repeat))
		;;choose the least complex leading coefficient to divide by
		(setq eqns-rep
		      (sort-key (subseq eqns-rep 0 2) '<
				#'(lambda (u var) (pcomplexity
						    (leading-coefficient u var)))
				repeat))))
	 (setq f (second eqns-rep))
	 (show repeat)
	 (multiple-value-bind (zl-REM c-reqd)
	     (gen-prem  f (first eqns-rep) repeat)
	   (push f *used-divisors*)
	   (mshow f (first eqns-rep))
	   (setq new-eqns (delete f (copy-list eqns)))
	   (cond (($zerop zl-REM) nil)
		 (t  (setq new-eqns  ;;these have f replaced by rem
			   (append new-eqns (list zl-REM)))))
	   (setq gg (nplcm open-g (ldata-inequality ldata)))
	   (cond ((may-invertp c-reqd  gg)
		  (setq ld2 (make-ldata :eqns  new-eqns
					:inequality  gg :variables vars )))
		 ;;make two ldata one where c-reqd is invertible  and f replaced by rem
		 ;; and other where its zero and f is still there.
		 (t (setq ld2 (make-ldata :eqns new-eqns
					  :inequality (nplcm gg c-reqd)
					  :variables vars))
		    (setq ld1 (make-ldata  :eqns
					   (cons c-reqd eqns)
					   :inequality gg
					   :variables vars))))
	   (setq answer

		 (cond
		   (ld1
			(format t "~%Breaking into dichotomy on:")
			(sh c-reqd)
			(format t "%original ldata followed by consequents:" )
			(des ldata)(des ld1) (des ld2 )
;	      (loop for v in (list ld1 ld2)
;		    when (not  (unit-idealp (ldata-eqns v) (ldata-inequality v)))
;		    appending (ldata-simplifications v )))

			(append (ldata-simplifications ld1
						       :open-g open-g :recursive-p t)
				(ldata-simplifications
				  ld2 :open-g open-g :recursive-p t)))
		       (t
			(format t "~%The c-reqd was invertible: ")
			(sh c-reqd)
			(iassert (not (member f (ldata-eqns ld2) :test #'equal)))
			(des ld2)
			(ldata-simplifications
			  ld2 :open-g open-g :recursive-p t))))))
	 (t (setq answer  (list ldata)))))
    (setq *used-divisors* nil))

    answer)


;
;;;;;the following was the divide-dichotomy in force at XMAS 84
;(defun  divide-dichotomy (ldata &key (open-g 1) &aux answer  used
;			  (eqns (ldata-eqns ldata)) f new-eqns
;			  occurs vars highest-vars orig-rep repeat eqns-rep  gg ld1 ld2)
;
;  "endeavors to turn ldata into an  triangular list of eqns
; so that each equation has  possibly one more variable occurring than the
; previous.  It takes a good order for the variables and then takes the first variable
; to be highest in two succeeding eqns, and does a division to try to correct this.  If
; the leading variable is not invertible it does a dichotomy "
;
;  ;;ordering should take into account open-g
;
;  (setq ldata (copy-list-structure ldata))
;  (setq vars (copy-tree  (good-order-variables eqns)))
;  (setq occurs (second vars))
;  (setq vars (first vars))
;  (setq highest-vars
;	(loop for v in occurs
;	      collecting (loop for u in vars
;			       when (member u v :test #'eq)
;			       do (return u))))
;  (show vars highest-vars)
;  (setq repeat
;	(loop named rep for v in vars
;	      do
;	      (loop for w on highest-vars
;		    when (and (eq (car w) v)
;			      (member v (cdr w) :test #'eq))
;		    do (return-from rep v))))
;
;  (unwind-protect
;    (progn
;      (cond
;	(repeat
;	 (setq eqns-rep  (loop for v in eqns
;			       for u in highest-vars
;			       when (eq u repeat)
;			       collecting v))
;	 (setq orig-rep (copy-list eqns-rep))
;	 (show (length eqns-rep))
;	 (setq eqns-rep (sort-key eqns-rep '< 'pdegree repeat))
;	 (setq used  (cons repeat (pdegree (first eqns-rep) repeat)))
;;         (push repeat *used-divisors*)
;	 (show (length eqns-rep))
;	 (cond ((eq ( pdegree (first eqns-rep) repeat)
;		    (pdegree (second eqns-rep) repeat))
;		;;choose the least complex leading coefficient to divide by
;		(setq eqns-rep
;		      (sort-key (firstn 2 eqns-rep) '<
;				#'(lambda (u var) (pcomplexity
;						    (leading-coefficient u var)))
;				repeat))))
;	 (setq f (second eqns-rep))
;	 (show repeat)
;	 (multiple-value-bind (rem c-reqd)
;	     (gen-prem  f (first eqns-rep) repeat)
;	   (push f *used-divisors*)
;	   (mshow f (first eqns-rep))
;	   (setq new-eqns (delete f
;				  (copy-list eqns)))
;	   (cond (($zerop rem) nil)
;		 (t  (setq new-eqns  ;;these have f replaced by rem
;			   (append new-eqns (list rem)))))
;	   (setq gg (nplcm open-g (ldata-inequality ldata)))
;	   (cond ((may-invertp c-reqd  gg)
;		  (setq ld2 (make-ldata eqns  new-eqns
;					inequality  gg variables vars )))
;		 ;;make two ldata one where c-reqd is invertible  and f replaced by rem
;		 ;; and other where its zero and f is still there.
;		 (t (setq ld2 (make-ldata eqns new-eqns
;					  inequality (nplcm gg c-reqd)
;					  variables vars))
;		    (setq ld1 (make-ldata  eqns
;					   (cons c-reqd eqns)
;					   inequality gg
;					   variables vars))))
;	   (setq answer
;
;		 (cond
;		   (ld1
;			(format t "~%Breaking into dichotomy on:")
;			(sh c-reqd)
;			(format t "%original ldata followed by consequents:" )
;			(des ldata)(des ld1) (des ld2 )
;;	      (loop for v in (list ld1 ld2)
;;		    when (not  (unit-idealp (ldata-eqns v) (ldata-inequality v)))
;;		    appending (ldata-simplifications v )))
;
;			(append (ldata-simplifications ld1
;						       :open-g open-g :recursive-p t)
;				(ldata-simplifications
;				  ld2 :open-g open-g :recursive-p t)))
;		       (t
;			(format t "~%The c-reqd was invertible: ")
;			(sh c-reqd)
;			(iassert (not (member f (ldata-eqns ld2))))
;			(des ld2)
;			(ldata-simplifications
;			  ld2 :open-g open-g :recursive-p t))))))
;	 (t (setq answer  (list ldata)))))
;    (setq *used-divisors* nil))
;
;    answer)
;;attempt to repeat use of the variable order
;(defun  divide-dichotomy (ldata &key (open-g 1) &aux answer  used
;			  (eqns (ldata-eqns ldata)) f new-eqns
;			  occurs vars highest-vars orig-rep repeat eqns-rep  gg ld1 ld2)
;
;  "endeavors to turn ldata into an  triangular list of eqns
; so that each equation has  possibly one more variable occurring than the
; previous.  It takes a good order for the variables and then takes the first variable
; to be highest in two succeeding eqns, and does a division to try to correct this.  If
; the leading variable is not invertible it does a dichotomy "
;
;  ;;ordering should take into account open-g
;
;  (setq ldata (copy-list-structure ldata))
;  (setq vars  (good-order-variables eqns))
;  (setq occurs (second vars))
;  (setq vars (first vars))
;  (loop for v in *used-divisors*
;	do (setq vars (delete v vars)))
;  (setq highest-vars
;	(loop for v in occurs
;	      collecting (loop for u in vars
;			       when (member u v :test #'eq)
;			       do (return u))))
;  (show vars highest-vars)
;  (setq repeat
;	(loop named rep for v in vars
;	      do
;	      (loop for w on highest-vars
;		    when (and (eq (car w) v)
;			      (member v (cdr w) :test #'eq))
;		    do (return-from rep v))))
;
;  (unwind-protect
;    (progn
;      (cond
;	(repeat
;	 (setq eqns-rep  (loop for v in eqns
;			       for u in highest-vars
;			       when (eq u repeat)
;			       collecting v))
;	 (setq orig-rep (copy-list eqns-rep))
;	 (show (length eqns-rep))
;	 (setq eqns-rep (sort-key eqns-rep '< 'pdegree repeat))
;	 (setq used  (cons repeat (pdegree (first eqns-rep) repeat)))
;;         (push repeat *used-divisors*)
;	 (show (length eqns-rep))
;	 (cond ((eq ( pdegree (first eqns-rep) repeat)
;		    (pdegree (second eqns-rep) repeat))
;		;;choose the least complex leading coefficient to divide by
;		(setq eqns-rep
;		      (sort-key (firstn 2 eqns-rep) '<
;				#'(lambda (u var) (pcomplexity
;						    (leading-coefficient u var)))
;				repeat))))
;	 (setq f (second eqns-rep))
;	 (show repeat)
;	 (multiple-value-bind (rem c-reqd)
;	     (gen-prem  f (first eqns-rep) repeat)
;	   (push f *used-divisors*)
;	   (mshow f (first eqns-rep))
;	   (setq new-eqns (delete f
;				  (copy-list eqns)))
;	   (cond (($zerop rem) nil)
;		 (t  (setq new-eqns  ;;these have f replaced by rem
;			   (append new-eqns (list rem)))))
;	   (setq gg (nplcm open-g (ldata-inequality ldata)))
;	   (cond ((may-invertp c-reqd  gg)
;		  (setq ld2 (make-ldata eqns  new-eqns
;					inequality  gg variables vars )))
;		 ;;make two ldata one where c-reqd is invertible  and f replaced by rem
;		 ;; and other where its zero and f is still there.
;		 (t (setq ld2 (make-ldata eqns new-eqns
;					  inequality (nplcm gg c-reqd)
;					  variables vars))
;		    (setq ld1 (make-ldata  eqns
;					   (cons c-reqd eqns)
;					   inequality gg
;					   variables vars))))
;	   (setq answer
;		 (cond (ld1
;			(format t "~%Breaking into dichotomy on:")
;			(sh c-reqd)
;			(format t "%original ldata followed by consequents:" )
;			(des ldata)(des ld1) (des ld2 )
;;	      (loop for v in (list ld1 ld2)
;;		    when (not  (unit-idealp (ldata-eqns v) (ldata-inequality v)))
;;		    appending (ldata-simplifications v )))
;
;			(append (ldata-simplifications ld1
;						       :open-g open-g :recursive-p t)
;				(ldata-simplifications
;				  ld2 :open-g open-g :recursive-p t)))
;		       (t
;			(format t "~%The c-reqd was invertible: ")
;			(sh c-reqd)
;			(iassert (not (member f (ldata-eqns ld2))))
;			(des ld2)
;			(ldata-simplifications
;			  ld2 :open-g open-g :recursive-p t))))))
;	 (t (setq answer  (list ldata)))))
;    (setq *used-divisors* nil))
;
;    answer)


;;evaluates the key function only once for each term
;;orders by (pred (key u) (key v)) true ==> u earlier than v in sorted list

(defun triangularp (ldata &aux vars (eqns (ldata-eqns ldata)) occurs highest-vars repeat)
  (setq vars  (good-order-variables eqns))
  (setq occurs (second vars))
  (setq vars (first vars))
  (loop for v in *used-divisors*
	do (setq vars (delete v vars :test #'equal)))
  (setq highest-vars
	(loop for v in occurs
	      collecting (loop for u in vars
			       when (member u v :test #'eq)
			       do (return u))))
  (show vars highest-vars)
  (setq repeat
	(loop named rep for v in vars
	      do
	      (loop for w on highest-vars
		    when (and (eq (car w) v)
			      (member v (cdr w) :test #'eq))
		    do (return-from rep v)))))

(defun sort-expensive-key (a-list pred key &aux clist)
  (declare (special pred))
  (setq clist (copy-list a-list))
  (loop for v on clist do (setf (car v)
				 (cons  (funcall key (car v)) (car v))))
  (setq clist (sort clist #'(lambda (u v) (funcall pred (car u) (car v)))))
  (loop for v on clist do (setf (car v) (cdar v)))
  clist)

;;one should do divide dichot on the simplest functions and
;;the lowest degree variables..

;(defun second-divide-dichotomy (ldata &key (open-g 1)  &aux tem
;				variables-to-exclude non-lin-eqns)
;  (unwind-protect
;    (progn
;      (loop for eqn in (ldata-eqns ldata)
;	    when (setq tem (any-linearp eqn open-g :variables-to-exclude
;					variables-to-exclude))
;	    do (push tem variables-to-exclude)
;            and
;	    collecting  eqn into lin-eqn
;	    else
;	    collecting eqn into non-lin
;	    finally (setq non-lin-eqns non-lin))
;      (setq non-lin-eqns     (sort-expensive-key
;			       non-lin-eqns #'< #'gen-pcomplexity))
;      (loop for eqn in non-lin-eqns
;	    collecting (degree-one-variables eqn) into deg-1
;	    collecting (list-variables eqn) into all-vars
;	    finally
;	    (setq deg1-divisions
;	    (loop for d1 in  deg-1
;		  for f in non-lin-eqns
;		  for i from 0
;		  appending
;		  (loop for al in all-vars
;			for j from 0
;			for g in non-lin-eqns
;			when (and (not (eql i j))
;				  (setq tem(intersect d1 al)))
;			collecting (list tem g f ) into possible-1)))
;	    (setq higher-divisions
;	    (loop for d1 in  all-vars
;		  for f in non-lin-eqns
;		  for i from 0
;		  appending
;		  (loop for al in all-vars
;			for j from 0
;			for g in non-lin-eqns
;			when (and (not (eql i j))
;				  (setq tem(intersect d1 al)))
;			collecting (list tem g f ) into possible-1)))
;	    (setq
;	      some-quotients
;	      (loop for try in deg1-divisions
;		    appending
;		    (loop for va in (third try)
;			  when (not (member (cons va (cdr try))
;					    *used-divisors*))
;			  collecting (append
;					     (multiple-value-list (gen-prem (first try)
;							   (second try)
;							   va)) try))))
;	    (loop for f in non-lin-eqns
;		  when (may-invertp (second v) open-g))))
;
;
;    (setq *used-divisors* nil)))

(defun linear-dichotomy (ldata &key (open-g 1) in-linear-dich &aux answer varl fns ans1 ans2)
  (mshow ldata)
  (cond
    (in-linear-dich (list ldata))
    ((multiple-value-setq (varl fns) (linear-solvedp (ldata-eqns ldata) :order-functions t))
     (cond ((linear-ldatap ldata :open-g open-g) (list ldata))
	   (t (loop for va in varl
		 for f in fns
		 with answ = 1
		 do (setq answ (ptimes answ (pcoeff f (list va 1 1))))
		 finally
		   (return
		      (cond ((may-invertp answ open-g)(list ldata))
			    (t
			     (setq answer
				   (append
				    (setq ans1
					  (ldata-simplifications
					   (make-ldata :eqns (ldata-eqns ldata)
						       :inequality (sftimes answ
									    (ldata-inequality
									     ldata)))
					   :open-g open-g :recursive-p t))
				    (setq ans2 (ldata-simplifications
						(make-ldata :eqns (cons answ (ldata-eqns ldata))
							    :inequality  (ldata-inequality
									  ldata))
						:open-g open-g :recursive-p t))))
			     (setq ans1
				   (loop for v in ans1
				      collecting
					(make-ldata :eqns
						    (contract-ideal-localization (ldata-eqns v)
										 (ldata-inequality v)))))
			     (setq answer (append ans1 ans2))
			     (setq answer (delete-redundant-ldata answer :gg open-g))
			     (mshow answer)
			     )))))))

    (t (format t "~%**Unchanged")
       (list ldata))))

(defun simplify-affine-ldata (ldata &key (open-g 1) &aux vari sheaf op)
  (setq vari (ml-sort (list-variables (ldata-eqns ldata)) ))
  (setq op (let ((*xxx* vari))(make-normal-zopen nil (length vari) open-g)))
  (setq sheaf (construct-pre-ldata-sheaves :opens (list op) :data (list (list ldata))))
  (simplify-svar-ldata sheaf))

(defvar *answer* nil)
(defun simplify-affine-ldata-write (ldata &key (open-g 1) (pathname "haskell:>wfs>answer.lisp") &aux answ)
  (setq *answer* (setq answ (simplify-affine-ldata ldata :open-g open-g)))
  (with-open-file (st pathname :direction :output)
    (let ((*standard-output* st) (*nopoint t) *print-radix*)
      (for-editor (des answ))
      (format st "~%(setq (answ (rerat '~A)))" answ))))
