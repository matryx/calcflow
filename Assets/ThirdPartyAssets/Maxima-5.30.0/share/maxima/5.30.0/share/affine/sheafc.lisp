;;; -*- Mode:Lisp; Package:CL-MAXIMA; Syntax:COMMON-LISP; Base:10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;(defun grobner-subset (id1 id2 &optional ( may-invert 1) &aux answ unit-ideal)
;  (declare   (values  answ unit-ideal))
;  (cond ((not (eql 1 may-invert))
;	 (setq *poly-simplifications* (grobner-remember (cons (pdifference
;								(ptimes (st-rat '$zzzzz)
;									may-invert)
;								1)
;							      id2))))
;	(t (setq *poly-simplifications* (grobner-remember id2))))
;  (setq answ
;	(cond (($zerop (polysimp 1))(setq unit-ideal 'unit-ideal))
; 	      (t
;	       (loop for v in id1 when (not ($zerop (polysimp v)))
;		     do (return nil)
;		     finally (return t)) )))
;  (values answ unit-ideal))

(defun grobner-subset (id1 id2 &optional ( may-invert 1) &aux answ unit-ideal)
;  (declare   (values  answ unit-ideal))
  (setq unit-ideal (unit-idealp id2 may-invert))
  (setq answ (loop for v in id1 when (not ($zerop (polysimp v)))
					    do (return nil)
			     finally (return t)) )
  (values answ unit-ideal))

;(defun unit-idealp (list-eqns &optional (may-invert 1) &aux )
;  (cond ((not (eql 1 may-invert))
;	 (setq *poly-simplifications* (grobner-remember (cons (pdifference
;								(ptimes (st-rat '$zzzzz)
;									may-invert)
;								1)
;							      list-eqns))))
;	(t (setq *poly-simplifications* (grobner-remember list-eqns))))
;  (cond (($zerop (polysimp 1)) 'unit-ideal)))

(defvar *invert-separately* t)

(defun fast-grobner-basis (list-eqns &optional (may-invert 1) (string-for-inverse-name "")
			    &aux eqns facts newvars inverses
		    (pcomplexity-of-inverses 80))
;  (declare    (values   *poly-simplifications* newvars))
   (cond
    ((loop for v in list-eqns
	   when  (may-invertp v may-invert)
	   do (setq *poly-simplifications* (list 1 (rzero)))  (return 'done)))
    ((not (numberp may-invert))
     (cond ((and *invert-separately* (> (pcomplexity may-invert)
					pcomplexity-of-inverses))
	    (setq facts (npfactor may-invert)))
	   (t (setq facts (list (square-free may-invert) 1))))
     (setq newvars (unused-variables (truncate (length facts) 2) string-for-inverse-name))
     (setq inverses
	   (loop for (pol deg) on facts by #'cddr
		 for new in newvars
		 when (< (pcomplexity pol) pcomplexity-of-inverses)
		 collecting
		 (pdifference (ptimes (list new 1 1)
				      pol) 1)
		 else do
		 (format t "~3%*******  That inverse was a little to complex to use: .. ****~3%")
		 (des pol)))
     (setq eqns (sort (copy-list (append inverses  list-eqns)) 'alphalessp))
     (setq *poly-simplifications* (grobner-remember eqns)))
    (t (setq *poly-simplifications* (grobner-remember (sort (copy-list list-eqns) 'alphalessp)))))
   (values   *poly-simplifications* newvars))


(defvar *invalid-contractions* nil)
(defun contract-ideal-localization (list-eqns may-invert &aux simps newvars)
    (catch 'took-too-long
      (multiple-value-setq (simps newvars) (fast-grobner-basis list-eqns may-invert 'zzzzzzzz)))
    (cond ((null simps) (format t "~%******Invalid contraction")
	   (push (list list-eqns may-invert) *invalid-contractions*) list-eqns)
	  (t
    (loop for v in (poly-relations-from-simplifications)
	  when (loop for var in newvars
		     when (not (zerop (pdegree v var)))
		     do (return nil) finally (return t))
	  collecting v))))

(defvar *too-long-for-grobner-basis* nil)
(defun unit-idealp (list-eqns &optional (may-invert 1) &aux answer)

  "Has a side effect of setting the *poly-simplifications* to be the correct ones"
  (format t "~%Entering unit-ideal")
  (if-verbose (show (gen-pcomplexity list-eqns) (pcomplexity may-invert)))
  (cond
    ;;this is handled by grobner-remember now
;    ((member (list list-eqns may-invert) *too-long-for-grobner-basis*)
;	 (setq *poly-simplifications* nil))
	(t
  (setq answer (catch 'took-too-long  (fast-grobner-basis list-eqns may-invert "ZZ")))
  (cond ((eq answer 'took-too-long) (push (list list-eqns may-invert) *too-long-for-grobner-basis*)
					  (setq *poly-simplifications* nil))
	(($zerop (polysimp 1)) 'unit-ideal)))))

(defun ptruncate (poly var deg)
  (cond ((<= (pdegree poly var) deg)
	 poly)
	(t (ptruncate1 poly var deg))))
(defun ptruncate1 (poly var degree &aux answ tem)
  (cond ((atom poly) poly)
	((pointergp (p-var poly) var)
	 (loop for (deg cof) on (cdr poly) by #'cddr
	       when (not (pzerop (setq tem
				       (ptruncate1 cof var degree))))
	       collecting deg into pol
	       and
	       collecting tem into pol
	       finally (return (cond (pol (cons (p-var poly) pol))
				     (t 0)))))
	((eq (p-var poly) var)
	 (loop for v on (cdr poly) by #'cddr
	       when (<= (car v) degree)
	       do (return (setq answ v)))
	 (cond (answ (cons var answ))
	       (t 0)))
	(t poly)))

(defun sort-key (data pred key &rest rest-key-arg)
  (declare (special rest-key-arg key pred))
  (sort (copy-list data) #'(lambda (u v)
		 (funcall pred (apply key u rest-key-arg)
			  (apply key v rest-key-arg)))))

(defun leading-coefficient (poly var)
  (pcoeff poly (list var (pdegree poly var) 1)))


;;condition on domain of op1-->op2
;; is that the  part of the denom of the map op1-->op2  which is relatively
;;prime to the inequality on op1, should be invertible on op2.
;;condition on the range is that the image of a point in op1
;;should satisfy the inequality of op2
(defremember open-subsetp (op1 op2 )
  (let ( map12 map21 gg gf domain-ok)
    (setq map21  (find-ring-map op2 op1))
    (setq gf (numerator (apply-rmap map12 (rmap-denom map21))))
    (setq map12 (find-ring-map op1 op2))
    (setq gg (numerator (apply-rmap map12 (zopen-inequality op1))))
    (cond ((may-invertp gf (ptimes gg (zopen-inequality op2))) (setq domain-ok t))
	  (t nil))
    ;;check the image lands in
    (cond ((and domain-ok
		(may-invertp (numerator (apply-rmap map21 (zopen-inequality op2)))
			     (zopen-inequality op1))) t)
	  (t nil))))

;
;  (setq gg (nplcm (numerator  (apply-rmap map (zopen-inequality op2) )) (rmap-denom map)))
;  (cond ((may-invertp gg (zopen-inequality op1)) t)
;	(t nil)))
;;works but makes to many refinements
(defun open-refinement-for-list (zopen list-eqns
				 &aux ( gg  (zopen-inequality zopen)) tem answ )
  (format t "Should be using best open cover see file")
  (cond ((null list-eqns)(list zopen))
	(t
	 (loop for pol on list-eqns
	       when (and (not (any-linearp (car pol)gg))
			 (setq tem (gm-prepared (car pol) :inequal gg)))
	       do (setq answ (open-refinement zopen (car pol)  (length tem)))
	       (return (loop for vv in answ
			     appending (open-refinement-for-list vv (cdr pol))))
	       finally (return (list zopen))))))


;;;
(defun open-refinement-for-list (zopen list-eqns
				 &aux ( gg  (zopen-inequality zopen)) tem answ  refs)
  (cond ((null list-eqns)(list zopen))
	(t
	 (loop for pol in list-eqns
	       when (and (not (any-linearp  pol gg))
			 (setq tem (gm-prepared  pol :inequal gg)))
	       do
	       (setq answ (best-open-cover list-eqns (zopen-inequality zopen)))
	       (iassert (unit-idealp answ (zopen-inequality zopen)))
	       (loop for gg in answ
		     when (not (or
				 (unit-idealp list-eqns gg)
				 (linear-ldatap (make-ldata :eqns list-eqns):open-g gg)))
		     do (fsignal "This refinement is not sufficient."))
	       ;;what here????

	       (format t "~%Refining for the equations ~/maxima::tilde-q-fsh/ with inequality ~/maxima::tilde-q-fsh/ ~
		    ~%Using inequalities ~/maxima::tilde-q-fsh/ " list-eqns gg answ)

	       (setq refs
		     (loop for gg in answ
			   collecting (zl-copy-structure zopen zopen-
						      inequality gg)))
	       ;;could stick something recursive in here to make linear variables in each
	       ;;eqn disjoint
;	      (return (loop for vv in refs
; 			    appending (open-refinement-for-list vv list-eqns)))
	       (return refs)
	       finally (return (list zopen))))))




;(defun eliminate-larger (a-list &key ( key 'identity) test test-not (destructive nil)
;			 &aux maybe-not)
;  (cond ((null destructive)(setq a-list (copy-list a-list))))
;  (cond (test-not (setq test test-not)
;		  (setq maybe-not 'null))
;	(t (setq maybe-not 'identity)))
;  (loop for v in a-list
;	      for i from 0
;	      appending (loop for  w in a-list
;			      for j from 0
;			      when (and (not (eql i j))
;					(funcall maybe-not (funcall test  (funcall key v)
;								 (funcall key w))))
;			      do (setq a-list (delete w a-list))))
;  a-list)

;;above does not seem to work
(defun eliminate-larger (a-list &key ( key 'identity) test test-not verify
			 (destructive nil)
			 &aux maybe-not answ orig)
  "Eliminates equal duplicates in A-LIST and then if test is given it
  eliminates items w such that (funcall test v w) is true for zl-SOME v not equal to w,
  and if test-not is specified it eliminates w such that (null (funcall test v w)) holds"
  (cond ((null destructive)(setq a-list (copy-list a-list))))
  (cond (test-not (setq test test-not)
		  (setq maybe-not 'null))
	(t (setq maybe-not 'identity)))
  ;;in case two things are equal we don't want to delete both!!
  (setq orig a-list)
  (setq a-list  (union-equal a-list))
  (setq answ (copy-list a-list))
  (loop for v in a-list
     for i from 0
     do (loop for w in a-list
	   for j from 0
	   when (and (not (eql i j))
		     (funcall maybe-not (funcall test  (funcall key v)
						 (funcall key w))))
	   do
	     (setq answ (delete w answ :test #'equal))))
  (cond (verify
	 (loop for v in orig
		do (loop for w in answ
			  when
			  (funcall maybe-not (funcall test  (funcall key w)
						      (funcall key v)))

			  do (return nil)
			  finally (merror "~A is not larger than anything in the answer ~a" v answ)))))
  answ)


(defun eliminate-smaller (a-list &key ( key 'identity) test test-not verify (destructive nil)
			  &aux maybe-not answ orig)
  "Eliminates equal duplicates in A-LIST and then if test is given it
  eliminates items w such that (funcall test v w) is true for zl-SOME v not equal to w,
  and if test-not is specified it eliminates w such that (null (funcall test v w)) holds"
  (cond ((null destructive)(setq a-list (copy-list a-list))))
  (cond (test-not (setq test test-not)
		  (setq maybe-not 'null))
	(t (setq maybe-not 'identity)))
  ;;in case two things are equal we don't want to delete both!!
  (setq orig a-list)
  (setq a-list  (union-equal a-list))
  (setq answ (copy-list a-list))
  (loop for v in a-list
     for i from 0
     do (loop for  w in a-list
	   for j from 0
	   when (and (not (eql i j))
		     (funcall maybe-not (funcall test  (funcall key v)
						 (funcall key w))))
	   do
	     (setq answ (delete v answ :test #'equal))))
  (cond (verify
	 (loop for v in orig
	    do (loop for w in answ
		  when
		    (funcall maybe-not (funcall test  (funcall key w)
						(funcall key v)))

		  do (return nil)
		  finally (merror "~A is not larger than anything in the answer ~a" v answ)))))
  answ)

(defun add-pls-zopen-history (pls)
  (loop for v in (pls-opens pls)
	for i from 0
	collecting (add-zopen-history v i) into opens
	finally (setf (pls-opens pls ) opens)))

(defun find-position-in-list (item list &key (test #'equal))
  (loop for v in list
	for i from 0
	when (funcall test v item)
	do (return i)))

(defun normalize-pls (pls  open ld-number &aux nth-open all-opens-data opens comp
		      lis-data MAPL tem  refs codimension-component)
  "makes the list-eqns become first coords in open and in the other opens, it
  should also look after refining if necessary: ie. if one of eqns is gm-prepared
  it must refine"
;  (declare (values normal-pls opens-not-to-blowup component-codimension))
  (setq opens (pls-opens pls))
  (setq lis-data  (pls-data pls))
  (cond ((numberp open)(setq nth-open open))
       (t (setq nth-open(find-position-in-list open opens))))
  (setq comp (match-components pls nth-open :ldata-number ld-number))
  (setq codimension-component (length (ldata-eqns (nth ld-number
						       (nth nth-open lis-data)))))
  (setq comp
	(loop for v in comp
	when (eq (cdr (assoc nth-open v :test #'equal)) ld-number)
	do (return v)))

  (loop for op  in opens
	for lis-ld in lis-data
	for i from 0
	when (setq ld-number  (cdr (assoc i comp :test #'equal)))
	do
	(setq refs (open-refinement-for-list op (ldata-eqns (nth ld-number lis-ld))))
	(loop for op1 in refs
	      do (push (list op1  lis-ld (ldata-eqns (nth ld-number lis-ld))) all-opens-data))
	else
	do  (push (list op  lis-ld nil) all-opens-data))
;  (setq all-opens-data  (eliminate-larger all-opens-data :key 'car
;					  :test 'open-subsetp ))
  (setq all-opens-data (nreverse  all-opens-data))
  (loop for v in all-opens-data
	for i from 0
	when (third v)
	collecting (setq tem (normalize-zopen (car v ) (third v))) into all-opens
	and
	do (setq MAPL (find-ring-map (car v) tem))
	and
	collecting (loop for ld in (second v)
			 collecting (apply-rmap MAPL ld))
	into data
	else
	collecting i into opens-not-to-blowup
	and
	collecting (car v) into all-opens
	and collecting (second v) into data
	finally (return (values
			  (make-pre-ldata-sheaves
				     :s-var	 (make-s-var :zopens  all-opens)
				     :data  data)
			  opens-not-to-blowup
			  codimension-component))))

(defun a-factor (fact-list1 fact-list2 &aux tem)
  (loop for (pol deg) on fact-list1 by #'cddr
	when (not  (and (setq tem (get-alt pol fact-list2))
			(>= tem deg)))
	do (return nil)
	finally (return t)))

(defun multiply-out-factorization (facts)
  (loop for (pol deg) on facts by #'cddr
     with answ = 1
     do (setq answ (ptimes answ (pexpt pol deg)))
     finally (return answ)))

(defun monomialp (pol)
  (cond((atom pol) t)
       (t (and (eq (length pol) 3) (monomialp (third pol))))))
(defun one-variablep (f)
  (and (eql (length f) 3) (numberp (third f))))

(defvar *leave-unit-ldata* nil)
(defun ldata-simplify-homogeneous (ldata &optional (may-invert 1) &aux all-others used  eqns facts answ simpler (added t) eqn tem)
  (process-sleep 15)
  (setq facts (loop for v in (ldata-eqns ldata)

		    do   (process-sleep 15)
		    when (not ($zerop v))
		    collecting (setq tem  (non-constant-factors v may-invert))
		    ))
		    ;;when (null tem) do (mshow v) (break 'here)))
  (loop for v in facts
	when (null v)
	do
	(return (setq answ 'unit)))
  (cond
    ((null answ)  (setq facts (eliminate-larger facts :test 'a-factor :verify t))
     (loop for v in facts
	   when (loop for  (pol deg) on v by #'cddr
		      when (not (one-variablep pol))
		      do (return nil)
		      finally (return t))

	   collecting (multiply-out-factorization v) into tem
	   else collecting (multiply-out-factorization v) into others
	   finally (setq used tem) (grobner-basis tem) (setq all-others others))
     (loop named sue while added
	   for ii from 0
	   do
	   (setq added nil)
	   (loop for w in all-others
		 do (setq eqn (numerator (polysimp w)))
		 when (not ($zerop eqn))
		 do
		 (cond ((numberp eqn)   (setq answ 'unit) (return-from sue 'done))
		       ((monomialp eqn)
			(add-to-poly-simplifications eqn) (push eqn used)
			(setq added t) (return 'again)))
		 and
		 collecting eqn into tem
		 when (may-invertp eqn may-invert) do (return-from sue (setq answ 'unit))
		 finally (setq simpler tem))
	   finally (setq eqns (append  (delete 0 simpler :test #'equal) used)))))
  (setq answ  (cond ((eq answ 'unit) (format t "~%It turned out to be trivial")
		     (cond (*leave-unit-ldata* (list (make-ldata :eqns '(1))) )))
		    (t (format t "~%There are ~A equations of complexity ~A of which ~A ~
				   are monomials"
			       (length eqns) (gen-pcomplexity eqns) (length used))
		     (list (zl-copy-structure ldata ldata- eqns eqns))))))


;;;try to do without factoring.
;(defun ldata-simplify-homogeneous (ldata &optional (may-invert 1)
;				   &aux all-others used  eqns facts answ)
;  (process-sleep 15)
;  (setq facts (loop for v in (ldata-eqns ldata)
;		    do   (process-sleep 15)
;		    collecting (non-constant-factors v may-invert)))
;  (loop for v in facts
;	when (null v)
;	do (return (setq answ 'unit)))
;  (cond ((null answ)  (setq facts (eliminate-larger facts :test 'a-factor))
;	 (loop for v in facts
;	       when (loop for  (pol deg) on v by #'cddr
;			  when (not (one-variablep pol))
;			  do (return nil)
;			  finally (return t))
;
;	       collecting (multiply-out-factorization v) into tem
;	       else collecting (multiply-out-factorization v) into others
;	       finally (setq used tem) (grobner-basis tem) (setq all-others others))
;	 (loop for w in all-others
;	       collecting (numerator (polysimp w)) into tem
;	       finally (setq eqns (append  (delete 0 tem) used)))))
;  (cond ((eq answ 'unit) (format t "~%It turned out to be trivial")nil )
;	(t
;	 (list (copy-structure ldata ldata- eqns eqns)))))




;;if the ldata on one open are trivial it will give empty list corresponding to that open.
(defun simplify-svar-homogeneous (pls &aux  all-data tem)
  (setq pls (copy-list-structure pls))
  (setq all-data
	(loop
	   for op in (pls-opens pls)
	   for w in (pls-data pls)
	   collecting (cons op
			    (loop for ld in w
			       when (setq tem (ldata-simplify-homogeneous ld (zopen-inequality op)))
			       appending tem))))
  (construct-pre-ldata-sheaves  :s-var (make-s-var :zopens (mapcar #'car all-data))
				:data (mapcar #'cdr all-data)))

(defun intersection-inequality-in-op1 (op1 op2 &aux hh )
  (let  ((map1 (find-ring-map op1 op2))
	 (map2 (find-ring-map op2 op1)))
    (setq hh (apply-rmap-to-square-free-factors map2 (zopen-inequality op2)))
    (setq hh (sftimes
	       hh (apply-rmap-to-square-free-factors map2 (rmap-denom map1))))
    (setq hh (sftimes  hh (zopen-inequality op1)))
    (setq hh (sftimes hh (rmap-denom map2)))))

(defun refine-pls (pls list-eqns &aux refs changed new-pls)
  (loop for op in (pls-opens pls)
	for lis in list-eqns
	for lis-dat in (pls-data pls)
	appending
	(progn
	  (setq refs (open-refinement-for-list op lis ))
;	  (loop for ope in refs
;	       (cond ((and lis original-open
;			(null (unit-idealp lis (zopen-inequality ope)))
;			(null (linear-ldatap
;				(make-ldata eqns lis)
;				:open-g  (zopen-inequality ope))))
;		      (if tried (fsignal "this won't work"))
;		      (setq hh (intersection-inequality-in-op1 ope original-open))
;		      (setq tem1 (try-harder-and-intersect
;				   op
;			      (make-ldata eqns eqns1) lis-dat hh))
;		      (setq lis (ldata-eqns tem1)) (setq tried t)
;		      (format t "~%*****Had to intersect ~VQ with original ldata to obtain ~VQ" eqns1 'fsh tem1 'fsh)

	       (cond ((> (length refs ) 1)(setq changed t)))
	       refs)  into all-ops
	appending
	(make-list (length refs) :initial-element lis-dat  ) into all-dat
	appending
	(make-list (length refs) :initial-element lis  ) into the-eqns
	finally
	(setq new-pls(construct-pre-ldata-sheaves :opens all-ops :data all-dat))
	(cond (changed (add-pls-zopen-history new-pls)))
	(return
	  (values new-pls
		  the-eqns ))))

(defun all-linearp ( eqns &optional (inequal 1))
  (loop for v in eqns when (not (any-linearp v inequal))
	do (return nil) finally (return t)))

;(defun prepare-for-blowup (pls open-number eqns &aux tr-data ld from-open ref-pls norm-op
;			   transl-eqns)
;  "the eqns will be translated around to other opens and the coords and ldata will be
; normalized on the components it meets.   A pls and opens-not-to-blowup are returned"
;  (setq from-open (nth open-number(pls-opens pls)))
;  (setq ld  (make-ldata eqns eqns))
;  (loop for op in (pls-opens pls)
;	for op-number from 0
;	collecting
;	(ldata-eqns
;	  (translate-reduced-component open-number op-number ld pls)) into lis-eqns
;	finally (multiple-value ( ref-pls transl-eqns)
;		  (refine-pls pls lis-eqns)))
;  (loop for op in (pls-opens ref-pls)
;	for eqns in transl-eqns
;	for i from 0
;	for lis-dat in (pls-data ref-pls)
;	when (and eqns lis-dat (all-linearp eqns (zopen-inequality op)))
;	do
;	(multiple-value
;	   (norm-op tr-data)
;	      (normalize-zopen op eqns :data lis-dat))
;	and collecting (cons norm-op tr-data) into all-data
;	else
;	collecting (cons op lis-dat) into all-data
;	and collecting i into opens-not-to-blowup
;	finally
;	(return(values (construct-pre-ldata-sheaves :opens (mapcar 'car all-data)
;				       :data (mapcar 'cdr all-data))
;		       opens-not-to-blowup))))
;;Better: the data of what to blow up may come from the associated reduced sheaf,
;;so to specify the data we should specify an open and the equations, and the
;;pls to blowup.

(defun zopen-special-subset (op1 op2)
   (and (equal (zopen-coord op1) (zopen-coord op2))
	(may-invertp (zopen-inequality op2) (zopen-inequality op1))))
(defun zopen-equal (op1 op2)
  (and (equal (zopen-coord op1) (zopen-coord op2))
       (equal (zopen-inequality op1) (zopen-inequality op2))))


(defun try-harder-and-intersect (op trans-ld lis-dat hh &aux answ  *refine-opens*)

  (setq answ (simplify-ldata (make-ldata :eqns (append (ldata-eqns trans-ld)
						      (ldata-eqns (car lis-dat))))
			     :open-g hh ))
  (fsignal 'hi)
  (format t "~%*** Having to try harder ***.")
  (cond ((equal(length answ) 1) (setq answ (car answ)))
	((null answ) nil)
	(t (fsignal "Two components in the translate")))
  (cond ((linear-ldatap answ :open-g (zopen-inequality op)) answ)
	(t (fsignal "Real trouble : the ldata are not linear after translation"))))

(defun prepare-for-blowup (pls open-for-equations eqns
			   &aux tr-data ld from-open ref-pls norm-op orig-eqns
			   transl-eqns int-inequal list-eqns tem)
  "the eqns will be translated around to other opens and the coords and ldata will be
 normalized on the components it meets.   A pls and opens-not-to-blowup are returned"
  (setq from-open open-for-equations)
  (setq orig-eqns eqns)
  (setq pls (copy-list-structure pls))
  (cond ((ldatap eqns)(setq ld eqns))
	(t   (setq ld  (make-ldata :eqns eqns))))
  (loop for op in (pls-opens pls)
	for op-number from 0
	for lis-dat in (pls-data pls)
	do (setq tem nil)
	when lis-dat
	collecting
	(progn
	  (multiple-value-setq (tem int-inequal)
			       (translate-reduced-component-and-reduce
				 from-open op ld
				 :homogeneous-ldata-on-to-open (car lis-dat)) )
	  (ldata-eqns tem))
	into lis-eqns
	else collecting nil into lis-eqns
	collecting int-inequal into lis-int-inequal
	when (and tem (not  (= (length (ldata-eqns tem)) (length (ldata-eqns ld)))))
	do (merror "wrong number of  equations")
	do
	(format t "~2%On open ~A the inequality is ~/maxima::tilde-q-fsh/ (before refinement).~
		   ~%The translated equations are ~/maxima::tilde-q-fsh/. "
		op-number (zopen-inequality op) (ldata-eqns tem))
;	when tem do (fsignal 'hi)

	finally (multiple-value ( ref-pls transl-eqns)
		  (refine-pls pls (setq list-eqns lis-eqns)))

	(setq ref-pls (copy-list-structure ref-pls)))
  (loop for op in (pls-opens ref-pls)
	for eqns in transl-eqns
	for i from 0
	for lis-dat in (pls-data ref-pls)
	when (and eqns lis-dat (all-linearp eqns (zopen-inequality op)))
	do
	(multiple-value
	  (norm-op tr-data)
	  (normalize-zopen op eqns :data lis-dat))
	(format t "~%On this open ~/maxima::tilde-q-fsh/ will correspond to ~/maxima::tilde-q-fsh/" eqns
		(loop for i from 1 to (length eqns) collecting (xxx i)))
	(check-normalization open-for-equations norm-op orig-eqns)
;	(break 'prepare1)
	(format t "~%normalizing open ~A . " i)
	and collecting (cons norm-op tr-data) into all-data
	else
	do
	(format t "%The open ~A does not meet" i)
						;(break 'prepare)
	and
	collecting (cons op lis-dat) into all-data
	and collecting i into opens-not-to-blowup
	finally
	(cond ((eq (length opens-not-to-blowup) (length all-data))
	       (merror "Not going to blowup anything")))
	(return(values (construct-pre-ldata-sheaves :opens (mapcar 'car all-data)
						    :data (mapcar 'cdr all-data))
		       opens-not-to-blowup ))))



(defun check-normalization (original-open  norm-op original-eqns &aux answ)
  (cond ((not (ldatap  original-eqns))(setq  original-eqns
					(make-ldata :eqns    original-eqns))))
  (setq answ (translate-component-and-reduce original-open norm-op
				  original-eqns))
  (setq answ (sort (copy-list (ldata-eqns answ)) 'alphalessp))
  (loop for v in answ
	for w in *xxx*
	when (not (and (eq (first v) w) (eql (second v) 1)))
	do (merror "bad normalization")))


(defun hhh  (pls reduced-pls op-num dat-num)
  (blowup-pls-and-reduced-pls pls reduced-pls (nth op-num (pls-opens reduced-pls))
			      (ldata-eqns (pls-ldata reduced-pls op-num :ldata-number dat-num))))
(defun blowup-pls-and-reduced-pls (pls reduced-pls opens-for-eqns eqns
				   &aux new-pls new-red-pls)
  (setq new-pls (blowup-pls pls opens-for-eqns eqns :simplify-homogeneous t))
  (setq new-red-pls (blowup-pls reduced-pls opens-for-eqns eqns
				:throw-out-components-in-exceptional-divisor t))
  (list new-pls (simplify-svar-ldata new-red-pls)))

(defun blowup-pls (pls open-for-eqns eqns  &key simplify-homogeneous
		   throw-out-components-in-exceptional-divisor &aux codim)
  (setq pls (copy-list-structure pls))
  (multiple-value-bind
    ( new-pls opens-not-to-blowup)
      (prepare-for-blowup pls open-for-eqns eqns)
    (show opens-not-to-blowup)
    (cond ((ldatap eqns)(setq codim (length (ldata-eqns eqns))))
	  (t (setq codim (length eqns))))

    (format t "~%****      Blowing a subscheme of codimension ~A  *** "codim )
    (des eqns)
    (blowup-sheaf new-pls codim :opens-not-to-blowup opens-not-to-blowup
		  :throw-out-components-in-exceptional-divisor
		  throw-out-components-in-exceptional-divisor
		  :simplify-homogeneous simplify-homogeneous)))

(defun blowup-pls (pls open-for-eqns eqns  &key simplify-homogeneous
		      add-exceptional-divisor-ldata &aux codim)
  (setq pls (copy-list-structure pls))
  (multiple-value-bind
    ( new-pls opens-not-to-blowup)
      (prepare-for-blowup pls open-for-eqns eqns)
    (show opens-not-to-blowup)
    (cond ((ldatap eqns)(setq codim (length (ldata-eqns eqns))))
	  (t (setq codim (length eqns))))
    (format t "~%****      Blowing a subscheme of codimension ~A  *** "codim )
    (des eqns)
    (blowup-sheaf new-pls codim :opens-not-to-blowup opens-not-to-blowup
		   :add-exceptional-divisor-ldata  add-exceptional-divisor-ldata
		  :simplify-homogeneous simplify-homogeneous)))


(defun blowup-keep-exceptional (pls  &key red-pls-list open-eqns-list &aux  open eqns
				ld-num op-num red-pls)
  (cond (red-pls-list (setq red-pls (first red-pls-list)
			    op-num (second red-pls-list)
			    ld-num (third red-pls-list ))
		      (setq open (nth op-num (pls-opens red-pls)))
		      (setq eqns (nth ld-num (nth op-num (pls-data red-pls)))))
	(t (setq open (first open-eqns-list))
	   (setq eqns (second open-eqns-list))))
  (let ((*leave-unit-ldata* t))
    (blowup-pls pls open  eqns  :simplify-homogeneous t
		:add-exceptional-divisor-ldata t)))
;(defun blowup-pls-by-component-of-reduced (pls open-number red-pls
;					   red-open-number ld-number &aux eqns)
;  (setq eqns (ldata-eqns
;	       (apply-rmap (find-ring-map
;			     (nth red-open-number (pls-opens red-pls))
;			     (nth open-number (pls-opens pls)))
;			   (nth ld-number (pls-ldata red-pls red-open-number :ldata-number
;						     ld-number)))))
;  (format t "~%Blowing up ") (des eqns) (format t "on open ~A" open-number)
;  (blowup-pls pls open-number eqns))

(defun blowup-pls-by-component-of-reduced (pls  red-pls
					   red-open-number ld-number &aux open eqns)
  (setq eqns  (nth ld-number      (nth red-open-number (pls-data red-pls))))
  (setq open (nth red-open-number (pls-opens red-pls) ))
  (format t "~%Blowing up ") (des eqns) (format t " with coordinates ")
  (des open)
  (blowup-pls pls  open
	     eqns))

;checks if the ldata eqns are in triangular linear form
;that is if there's a linear variable in eqn1 not occuring in remaining eqns
;etc.


(defun linear-ldatap (ldata  &key (open-g 1) &aux (eqns (ldata-eqns ldata))
		      var-occurs lin-vars tem)
  (block sue
;    (show eqns)
  (setq lin-vars (loop for v in eqns
	collecting (setq tem (all-linear-variables v open-g))
	when (null tem)do (return-from sue nil)))
; (show lin-vars)
  (setq var-occurs (mapcar 'list-variables eqns))
  (loop for v in lin-vars
	for w on var-occurs
	collecting
	(loop named kay for vv in v
	      do
	      ;;check vv doesn't occur in remaining equations
	      ;;and if not put it in collection
	      (loop for ww in (cdr w)
		    when (member vv ww :test #'eq) do (return nil)
		    finally (return-from kay vv))
	      finally (return-from sue nil)))))

(defun reduce-linear-ldata (ldata variables &key (open-g) &aux f eqns vars)
  (setq eqns (reverse (ldata-eqns ldata)))
  (setq vars (reverse variables))
  (loop while fns
	for v in vars
	with fns = eqns
	with used-up
	do
	(setq f (car fns))
	(setq fns (replace-functions fns   f v :invertible-g open-g))

	(push f used-up)
	finally (return used-up)))

;
;(defun try-harder-delete-redundant-components (lis-dat &optional invertible-g)
;  (setq codims  (loop for v in lis-dat
;	 collecting (length (ldata-eqns v))))
;  (loop for cod1 in codims
;	for ld1
;	when (and  (> max (apply 'max codims))
;		   (not (linear-ldatap ld1) :open-g invertible-g))
;	do
;	(setq simp-ld (ldata-simplifications ld1 invertible-g))
;	(loop for cod2 in codims
;	      for ld2 in lis-dat
;	      when ( > cod2 cod1)
;	      do (grobner-subset '(1) (ldata-eqns ld2) invertible-g)
;	      (loop for s-ld in simp-ld
;		    do
;		    (loop for fn in (ldata-eqns s-ld)
;			  when (null (any-linearp fn invertible-g))
;			  do (setq deg-1-vars (degree-one-variables fn))
;			  (loop for v in deg-1-vars
;				do (check-containment ld1 ld2 invert))
;

(defmacro setq-eqns ( &rest alt-list)
  (loop for v on alt-list by #'cddr
	collecting `(cond ((ldatap ,(second v))(setf  ,(car v ) (ldata-eqns ,(second v))))
			  (t (setf ,(car v) ,(second v))))
			    into tem
	finally (return (cons 'progn tem))))

;;tries to check that (var ld2) is subset (var ld1) by checking where dich-f is not zero
;;and where it is zero  .  If simplify is t it will try simplifying ld1 further
;;into components on the open where dich-f is not zero, and then try to verify
;;that (var ld2) is contained in one of the components so obtained.
;;this handles the example below where the first ideal has zero set contained
;;in the zero set of the second one but one still has to simplify the second one..
;ld2:[2*X1*X4*X6+X2*X4^2-X1^2*X3,X7,X8,X5,X6^2+X2*X3]
;ld1:[X5,X8,X4*X7-X4*X6+X1*X3,X7^2-X6^2-X2*X3]
;(SETQ LLD (RERAT (QUOTE ((LDATA ((X6 1 (X4 1 (X1 1 2)) 0 (X4 2 (X2 1 1) 0 (X3 1 (X1 2 -1)))) (X7 1 1) (X8 1 1) (X5 1 1) (X6 2 1 0 (X3 1 (X2 1 1)))) 1 0) (LDATA ((X5 1 1) (X8 1 1) (X7 1 (X4 1 1) 0 (X6 1 (X4 1 -1) 0 (X3 1 (X1 1 1)))) (X7 2 1 0 (X6 2 -1 0 (X3 1 (X2 1 -1))))) 1 0)))))




(defun check-containment (  ld1 ld2 dich-f &key (open-g 1) simplify
			  &aux eqn1 eqn2   lis-ld)
  (block  sue
    (setq-eqns eqn1 ld1 eqn2 ld2)
;  (setq cont (grobner-subset eqn1 eqn2  (nplcm open-g dich-f)))
;  (setq cont2 (grobner-subset (cons dich-f eqn1 ) (cons dich-f eqn2) open-g))
; (show cont2)

    (cond (simplify (setq lis-ld (ldata-simplifications ld1 :open-g (nplcm open-g dich-f))))
	  (t (setq lis-ld (list ld1))))
    ;;run through the splitting of lis-ld into components
    (loop for ld in lis-ld
	  when
	  (check-containment1 ld eqn2 dich-f :open-g open-g)
	  do (return-from sue t))))


(defun check-containment1 (  ld1 ld2 dich-f &key (open-g 1)
			  &aux eqn1 eqn2 cont cont2 )
  (setq-eqns eqn1 ld1 eqn2 ld2)
  (setq cont (grobner-subset eqn1 eqn2  (nplcm open-g dich-f)))
  (cond (cont
	 (setq cont2 (grobner-subset (cons dich-f eqn1 ) (cons dich-f eqn2) open-g))
	 cont2))
  (and cont cont2))

(defmacro sim-hom (x)`(setq  ,(intern (string-append "SIMP-" (string x)))
	  (simplify-svar-homogeneous ,x)))

(defmacro sim-ld (x)`(setq  ,(intern (string-append "RED-" (string-trim "SIMP-"
									(string x))))
	  (simplify-svar-ldata ,x)))

(defun sub-scheme (pls op-number ld-number)
  (construct-pre-ldata-sheaves :opens (list (nth op-number
							       (pls-opens pls)))
				     :data (list (list (nth ld-number
							    (nth op-number
								 (pls-data pls)))))))

(defun open-sub-scheme (pls &rest op-numbers)
  (loop for i in op-numbers
	collecting (nth i (pls-opens pls)) into ops
	collecting (nth i(pls-data pls) ) into dat
	finally
	(return (construct-pre-ldata-sheaves :opens ops
				     :data dat))))


(defun intersect-ldata (ld1 ld2 &key ( open-g 1) &aux eqns *refine-opens*)
  (setq eqns (append (ldata-eqns ld1) (ldata-eqns ld2)))
  (ldata-simplifications (make-ldata :eqns eqns ) :open-g open-g))

(defmacro save-rational (name &optional (file-name "r20:ps:<atp.schelter>kill.tem" aa))
  (cond ((null aa)
	 (setq file-name (merge-pathnames file-name))
	 (setq file-name (send file-name :new-name (string name)))))

  `(with-open-file  (st ',file-name :direction :output)
       (let (*print-gensym*)
     (format st ";;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp -*-
     ~%(setq ~A (rerat '" ',name) (format st "~s" ,name)
     (format st "))")
     ',file-name)))

(defun save-parts  ( obj file-name &aux  me)
  (with-open-file (st  file-name :direction :output)
    (let (*print-gensym*)
    (format st ";;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp; Mode: LISP -*- ~%")
    (loop for v in obj
	  for i from 1
	  do
	  (setq me `(setq ,(intern (format nil "PART~A" i)) (rerat ',v)))
	  (format st "~%;;next part ~%~S" me))
    (send st :pathname))))

(defun save-blew-up ( &aux  me)
  (with-open-file (st "r20:ps:<atp.schelter>blew-up.tem" :direction :output)
    (let (*print-gensym*)
    (format st ";;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp; Mode: LISP -*- ~%")
    (loop for v in *blew-up*
	  for i from 1
	  do
	  (setq me	      `(setq ,(intern (format nil "PART~A" i)) (rerat ',v)))
	  (format st "~%;;next part ~%~S" me))
    (send st :pathname))))




;(defun triangular (fns &key (open-g 1) variables-solved-for variables-in-gm-coefficients)
;  (cond ((null fns) 'ok)
;	((eql (length fns ) 1)
;	 (cond ((loop for var in (degree-one-variables (car fns))
;		      when (may-invertp (pcoeff (car fns) (list var 1 1)) open-g)
;		      do (return var)))
;	       (t (setq tem (nsubst nil 'ok (gm-all-prepared (car fns) open-g)))
;		  (loop named sue
;		    for possible in tem
;			do
;			(loop for var in possible
;			      when (not (member var variables-in-gm-coefficients :test #'eq))
;			      do (return-from sue 'ok-gm))))))
;	(t (loop for f in fns
;		     collecting (setq var-1 (degree-one-variables f)) into vars-1
;		     do
;		     (loop for va in var-1
;			   when (and (poly-linearp f va open-g)
;				     (setq tem (triangular (delete f (copy-list fns))
;						 :variables-solved-for 									  variables-solved-for
;						 :variables-in-gm-coefficients
;						 variables-in-gm-coefficients)))
;			   do (return (cons va tem)))


;;;will guarantee that the system has the form
;;  f1 (u1,   us,x1)
;;  f2 (u1,   us,x1,x2)
;;  ..
;;  fn (u1,   us,x1,x2,   xn)
;; and  xi is linear in fi and none of the variables in the  coefficient of xi in fi
;; occur among the x1,x2,x3,.. xi-1.  This will guarantee irreducibility, since if
;; we did xn>xn-1>..>x1>us>...>u1 then
;; the system is irreducible.  We show fi is irreducible modulo the f1,.. fi-1.
;; To see this: If it could be factored it would mean the coefficient of xi, say
;; ci and the constant di had a common factor.   The coefficient ci is reduced
;; withrespect to the preceding system, and so

(defun delete-nth ( n a-list)
  (loop for i from 0
	for v in a-list
	when
	(not (eql n i))
	collecting v))
;;will return a list of variables ANSWER occurring with degree 1 in various equations
;;such that the equations may be so reordered such that the
;;ith variable in Answer
;; u1+x1,
;; x1+u1*x2
;; Is such a system which is not irreducible.

(defun linear-triangularp (fns &key variables-solved-for variables-in-prev-eqns
			  varlist-of-fns &aux tem )
  (cond ((null varlist-of-fns)
	 (setq varlist-of-fns (loop for f in fns collecting (list-variables f)))))
  (cond ((null fns)variables-solved-for)
	(t
	 (loop named sue
	       for f in fns
	       for i from 0
	       for varl in varlist-of-fns
	       do (loop for va in varl
			when (and
			       (not (member va variables-solved-for :test #'eq))
			       (not (member va variables-in-prev-eqns :test #'eq))
			       (eq (pdegree f va) 1)
			       (setq tem  (linear-triangularp
						 (delete-nth i fns)
						 :varlist-of-fns
						 (delete-nth i varlist-of-fns)
						 :variables-solved-for
						 (cons va variables-solved-for)
						 :variables-in-prev-eqns
						 (append varl variables-in-prev-eqns))))
			do (return-from sue tem ))))))

;;returns a list of variables each of which occurs with degree 1 in exactly
;;one function and does not occur in any other fn.
(defun linear-solvedp (fns &key variables-solved-for variables-in-prev-eqns
			  order-functions
			  varlist-of-fns &aux tem )
  (cond ((null varlist-of-fns)
	 (setq varlist-of-fns (loop for f in fns collecting (list-variables f)))))
  (cond ((null fns)variables-solved-for)
	(t
	 (loop named sue
	       for f in fns
	       for i from 0
	       for varl in varlist-of-fns
	       do (loop for va in varl
			when (and
			       (not (member va variables-solved-for :test #'eq))
			       (not (member va variables-in-prev-eqns :test #'eq))
			       (not (loop for lis in varlist-of-fns
					  for j from 0
					  when (and  (not (eql i j))
						     (member va lis :test #'eq))
					  do (return t)))
			       (eq (pdegree f va) 1)
			       (setq tem
				 (linear-solvedp
						 (delete-nth i fns)
						 :order-functions order-functions
						 :varlist-of-fns
						 (delete-nth i varlist-of-fns)
						 :variables-solved-for
						 (cons va variables-solved-for)
						 :variables-in-prev-eqns
						 (append varl variables-in-prev-eqns))))
			do (return-from sue tem))))))

(defun linear-solvedp (fns &key variables-solved-for variables-in-prev-eqns order-functions varlist-of-fns &aux tem ord-fns)
  (cond ((null varlist-of-fns)
	 (setq varlist-of-fns (loop for f in fns collecting (list-variables f)))))
  (cond ((null fns)(values variables-solved-for (subst nil t order-functions)))
	(t
	 (loop named sue
		for f in fns
		for i from 0
		for varl in varlist-of-fns
		do (loop for va in varl
			  when (and
				(not (member va variables-solved-for :test #'eq))
				(not (member va variables-in-prev-eqns :test #'eq))
				(not (loop for lis in varlist-of-fns
					    for j from 0
					    when (and  (not (eql i j))
						       (member va lis :test #'eq))
					    do (return t)))
				(eq (pdegree f va) 1)
				(progn (multiple-value-setq
					   (tem ord-fns)
					 (linear-solvedp
					  (delete-nth i fns)
					  :varlist-of-fns
					  (delete-nth i varlist-of-fns)
					  :variables-solved-for
					  (cons va variables-solved-for)
					  :order-functions
					  (cond (order-functions
						 (cons f order-functions)))
					  :variables-in-prev-eqns
					  (append varl variables-in-prev-eqns)))))
			  do (return-from sue (values  tem ord-fns)))))))


(defun linear-solvedp (fns &rest ignore &aux bad good all-degs varl)
  (setq varl (list-variables fns))
  (let ((genvar (nreverse (sort varl 'pointergp))))
    (setq all-degs (loop for fn in fns collecting (pdegreevector fn)))
    (loop named kay
       for degs in all-degs
       for fn-number from 0
       do (loop named sue for i in degs
	     for j from 0
	     when (and (eql i 1) (not (member j bad :test #'eq)))
	     do
	       (loop for vv in all-degs
		  when (not (eql degs vv))
		  do (cond ((not (zerop (nth j vv) ))
			    (push j bad) (return 'done)))
		  finally (push j good) (return-from sue))
	     finally (return-from kay (setq good nil))))

    (loop for j in (nreverse good)
       collecting (nth j genvar))))

;(defun plain-simplify-svar-ldata (pls &aux *refine-opens* *stop-simplify* tem)
;  (loop for op in (pls-opens pls)
;	for lis in (pls-data pls)
;	do
;	(loop for ld in lis
;	      appending (ldata-simplifications ld :open-g (zopen-inequality op))
;	      into all-ld
;	      finally
;	      (setq tem (delete-redundant-ldata all-ld :gg (zopen-inequality op)
;					      :ignore-ldata-inequalities t)))
;	collecting tem into simp-ld
;	finally
;
;	(return
;	  (construct-pre-ldata-sheaves :data simp-ld :opens (pls-opens pls)))))



(defun plain-simplify-svar-ldata (pls &aux   list-of-dat simp-one-pls one-pls
				  ineq)
  (loop for op in (pls-opens pls)
	for i from 0
	for lis in (pls-data pls)
	do
	(setq one-pls (open-sub-scheme pls i))
	(setq simp-one-pls (simplify-svar-ldata one-pls))
	(cond ((eq (length (pls-opens simp-one-pls)) 1)
	       (setq list-of-dat (car  (pls-data simp-one-pls))))
	      (t (loop for ope in (pls-opens simp-one-pls)
		       for lis-dat in (pls-data simp-one-pls)
		       do (setq ineq (num (ratreduce (zopen-inequality ope)
						     (zopen-inequality op))))
		       appending
		       (loop for ld in lis-dat
			     collecting (make-ldata :eqns
						    (contract-ideal-localization
						      (ldata-eqns ld)
						      ineq)))
		       into all-dat
		       finally
		       (setq list-of-dat
			     (delete-redundant-ldata  all-dat :gg (zopen-inequality
									  op))))))
	collecting list-of-dat into all-data
	finally (return
		  (construct-pre-ldata-sheaves
		    :opens (pls-opens pls)
		    :data all-data))))


(defun svar-simp (ld &key (open-g 1))
  (simplify-svar-ldata (construct-pre-ldata-sheaves :opens (list (make-normal-zopen nil 8 open-g)) :data (list (list ld)))))

(defun jacobian (eqns &key ( variables (list-variables eqns))
		 ( codim (length eqns)) &aux det mat row-tables col-tables)
  (setq mat (matrix-rows (jacobian-matrix eqns variables)))
;  (displa (cons '($matrix) (loop for v in mat collecting (cons'(mlist)
;							     (mapcar 'new-disrep v)))))
;  (show variables)
  (setq row-tables (list-tableaux codim (length eqns)))
  (setq col-tables (list-tableaux  codim (length variables)))
  (loop named sue
	for ro-tabl in row-tables
	appending
	(loop for tabl in col-tables
	      do (setq *sparse-matrix* (convert-to-sparse-matrix
					 mat :columns-to-use tabl
					 :rows-to-use ro-tabl

					 :re-use-sparse-matrix  *sparse-matrix*))
	      (setq det (sp-determinant *sparse-matrix*))
	      when (numberp det)
	      do      (cond ((not ($zerop det)) (sp-show-matrix *sparse-matrix*)
			     (show tabl ro-tabl)
			     (return-from sue '(1))))
	      else collecting det into jacob
	      finally (return jacob))))


(defun rank-generic-reduce-jacobian (eqns &optional (variables(list-variables eqns)) &aux mat det )
;  (declare (values must-invert rank))
 (setq mat (matrix-rows (jacobian-matrix eqns variables)))
 (setq *sparse-matrix* (convert-to-sparse-matrix mat :re-use-sparse-matrix *sparse-matrix*))
 (setq det (sp-determinant *sparse-matrix*))
 (values det (sp-number-of-pivots *sparse-matrix*)))
(defun reduce-jacobian (eqns &optional (variables (list-variables eqns)) &aux mat det)
;  (declare (values must-invert rank))
 (setq mat (matrix-rows (jacobian-matrix eqns variables)))
 (setq *sparse-matrix* (convert-to-sparse-matrix mat :re-use-sparse-matrix *sparse-matrix*))
 (setq det (sp-determinant *sparse-matrix*))
 (values det (sp-number-of-pivots *sparse-matrix*)))
;;(non-singular-complete-intersection-p  (st-rat #$[x^2+w+y^3,u+y^2+x^2,w^2+y^2+y]$) )
;;the above has a 4 singular points given by the following equations:
;;the jacobian together with the equations is a mess to try and solve
;[2*Y+1,
;4*X^2+4*U+1,
;8*W-8*U-3,
;64*U^2+48*U-7]
(defun non-singular-complete-intersection-p (eqns &key (open-g 1)
					     (use-simplification t)
					     &aux new
					     singular-locus eqns-and-jacob)
;  (declare (values ( singular-locus non-singularp)))
  (cond ((ldatap eqns) (setq eqns (ldata-eqns eqns))))
  (setq eqns-and-jacob  (mapcar 'square-free (append eqns (jacobian eqns))))
  (cond (use-simplification
	 (setq new (ldata-simplifications
		     (make-ldata :eqns eqns-and-jacob) :open-g open-g))
	 (cond ((null new) (values '(1)  t))
	       (t  (values   new nil))))
	(t
	 (cond ((unit-idealp eqns-and-jacob open-g)
		(values	  '(1) t))
	       (t (setq singular-locus (poly-relations-from-simplifications ))
		  (values (list (make-ldata :eqns singular-locus)) nil))))))

(defun non-singular-complete-intersection-p (eqns &key (open-g 1) codim
					     (use-simplification t)
					     (variables (list-variables eqns))
					     &aux new
					     singular-locus eqns-and-jacob)
;  (declare (values ( singular-locus non-singularp)))
  (cond ((ldatap eqns) (setq eqns (ldata-eqns eqns))))
  (cond ((null codim) (setq codim (length eqns))))
  (setq eqns-and-jacob  (mapcar 'square-free (append eqns (jacobian eqns :variables variables
								    :codim codim))))
  (cond (use-simplification
	 (setq new (ldata-simplifications
		     (make-ldata :eqns eqns-and-jacob) :open-g open-g))
	 (cond ((null new) (values '(1)  t))
	       (t  (values   new nil))))
	(t
	 (cond ((unit-idealp eqns-and-jacob open-g)
		(values	  '(1) t))
	       (t (setq singular-locus (poly-relations-from-simplifications ))
		  (values (list (make-ldata :eqns singular-locus)) nil))))))
(defun simplify-dichotomy (ldata fn &key (open-g 1) &aux answ)
  (setq answ (append (ldata-simplifications (make-ldata :eqns (cons fn (ldata-eqns ldata) ))
					    :open-g open-g)
		     (loop for v in
			  (ldata-simplifications ldata :open-g (sftimes open-g fn))
			collecting
			  (make-ldata :eqns (contract-ideal-localization (ldata-eqns v) fn)))))
  (delete-redundant-ldata answ :open-g open-g :ignore-ldata-inequalities t))

(defun non-trivial-open-sub-scheme (pls &aux non-trivial)
  (setq non-trivial (loop for lis in (pls-data pls)
			  for i from 0
			  when (and lis (not (member 1 (ldata-eqns (car lis)) :test #'equal)))
			  collecting i))
  (show non-trivial) (break t)
  (apply #'open-sub-scheme pls non-trivial))

(defun show-divisors (pls &aux tem)
  (loop for lis in  (pls-data pls)
	for i from 0
	collecting (setq tem (loop for ld in (cdr lis)
			 when (not (numberp (setq tem (car (ldata-eqns ld)))))
			 collecting tem))
	do (format t "~%On open ~D the divisors are ~/maxima::tilde-q-fsh/" i tem)))

(defmacro alter-ldata (ldat &rest keys)
  (loop for (key repl)  on keys by #'cddr
	with u = '.ldat.
	collecting `(setf (,(intern (format nil "LDATA-~A" key)) .ldat.) ,repl) into body
	finally (cond (body (return `(let ((.ldat. ,ldat)) ,@body .ldat.)))
		      (t (return `(progn ,ldat))))))

(defun jacobian-dichotomy (ldata &key (open-g 1) &aux ld1 ld2 gg answer)
  (multiple-value-bind (c-reqd rank) (rank-generic-reduce-jacobian (ldata-eqns ldata))
    (cond ((may-invertp c-reqd (setq gg (sftimes open-g (ldata-inequality ldata))))
	   (cond ((eql (length (ldata-eqns ldata)) rank) (format t "~%Non singular ldata"))
		 (t (format t "~%singular ldata")))
	   (setq answer (list ldata)))
	  ((eql rank (length (ldata-eqns ldata)))
	   (setq ld1 (copy-list-structure ldata))
	   (setq ld2 (copy-list ldata))
	   (alter-ldata ld1 eqns (append (ldata-eqns ldata) (list c-reqd))
			inequality gg)
	   (alter-ldata ld2 inequality (sftimes gg c-reqd))
	   (format t "~%Breaking into dichotomy on:")
	   (sh c-reqd)
	   (format t "%original ldata followed by consequents:" )
	   (des ldata)(des ld1) (des ld2 )
	   (setq answer
		 (append (ldata-simplifications ld1 :open-g open-g :recursive-p t)
			 (ldata-simplifications ld2 :open-g open-g :recursive-p t))))
	  (t (setq answer (list ldata))))
    (delete-redundant-ldata answer :open-g open-g :ignore-ldata-inequalities t)))

(defun $list_components (lis-ldat)
  (cond ((eq (car lis-ldat) 'pre-ldata-sheaves)
	 (setq lis-ldat (apply 'append (pls-data lis-ldat))))
	((ldatap lis-ldat) (setq lis-ldat (list lis-ldat))))
  (cons '(mlist) (loop for v in lis-ldat
		    collecting (cons '(mlist) (mapcar 'new-disrep (ldata-eqns v))))))


(defun $simple_solve (eqns &key (open-g 1) &aux ld vari solu  )
  (setq ld (make-ldata :eqns (st-rat eqns)))
  (multiple-value-bind (to-invert rank) (rank-generic-reduce-jacobian (ldata-eqns ld))
    (setq to-invert (sftimes 1 to-invert))
    (cond((eql  (length (ldata-eqns ld)) rank)
	  (cond ((may-invertp to-invert open-g))
		(t(format t "having to invert ~/maxima::tilde-q-fsh/" to-invert)
		  (setq open-g (sftimes open-g to-invert))))))
    (setq vari (linear-ldatap ld :open-g (setq open-g(st-rat open-g))))
    (cond ((null vari) (fsignal "equations not linear-ldatap")))
    (setq solu (solve-simple-system (ldata-eqns ld) vari :invertible open-g))
    (cons '(mlist) (loop for v in vari
		      for w in solu
		      collecting (list '(mequal) (get v 'disrep)  (new-disrep (st-rat  w)))))))
