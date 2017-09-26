;;this program is derived from maxima/share/calculus/cartan.lisp
;;I add clifford operator &,&2
;;I add clifford differential operator d_c
  

(defprop $\| %\| verb) 
(defprop $\| "|" op) 
(putopr "|" '$\|)
(add2lnc (quote "|") $props) 
(defprop $\| dimension-infix dimension) 
(defprop $\| (32 124 32) dissym) 
(defprop $\| 120 lbp) 
(defprop $\| 180 rbp) 
(defprop $\| parse-infix led) 
(defprop $\| msize-infix grind) 
(defprop %\| dimension-infix dimension) 
(defprop %\|  (32 124 32) dissym) 
(mdefprop $\| ((lambda) ((mlist) $v $f) ((mprog simp) ((mlist simp)
 $i $j $ext101 $ext102 $ext103 $ext104) ((msetq simp) $ext103 
(($expand simp) $f)) ((msetq simp) $ext102 ((mtimes simp) 
(($v simp array) 1) (($coeff simp) $ext103 (($basis simp array) 1)))) 
((mdo simp) $i 2 nil nil $dim nil ((mprogn simp) ((msetq simp) 
$ext101 (($coeff simp) $ext103 (($basis simp array) $i))) 
((mcond simp) ((mnotequal simp) $ext101 0) ((msetq simp) $ext101
 (($substitute simp) (($extsub simp array) $i) $ext101)) t $false)
 ((msetq simp) $ext102 ((mplus simp) $ext102 ((mtimes simp) $ext101
 (($v simp array) $i)))))) ((mreturn simp) (($expand simp) $ext102))))
 mexpr) 
(add2lnc (quote (($\|) $v $f)) $functions) 
(defprop %\| $\| noun) 

(defprop $@ %@ verb) 
(defprop $@ "@" op) 
(putopr "@" '$@)
(add2lnc (quote "@") $props) 
(defprop $@ dimension-infix dimension) 
(defprop $@ (32 38 32) dissym) 
;;(defprop $@ 140 lbp) 
;;(defprop $@ 180 rbp)
(defprop $@ 80 lbp)
(defprop $@ 100 rbp) 
(defprop $@ parse-infix led) 
(defprop $@ msize-infix grind) 
(defprop %@ dimension-infix dimension) 
(defprop %@ (32 38 32) dissym)
;;exterior operator @ 
(mdefprop $@ 
	  ((lambda) 
	   ((mlist) $f $g) 
	   ((mprog simp) 
	    ((mlist simp) $i $j $ext101 $ext102 $ext103 $ext104 $ext105) 
	    ((msetq simp) $ext101 0) 
	    ((msetq simp) $ext102 $true) 
	    ((msetq simp) $ext103 
	     (($expand simp) $f)) 
	    ((mdo simp) $i $dim -1 nil 1 nil 
	     ((mprogn simp) 
	      ((msetq simp) $ext104 
	       (($expand simp) 
		(($bothcoef simp) $ext103 
		 (($basis simp array) $i)))) 
	      ((msetq simp) $ext105 
	       (($first simp) $ext104)) 
	      ((mcond simp) 
	       ((mnotequal simp) $ext105 0) 
	       ((mprogn simp) 
		((msetq simp) $ext103 (($last simp) $ext104))  
		((msetq simp) $ext101 
		 ((mplus simp) $ext101 
;;(($@ simp) $ext105
;;move $ext105 later 
		  (($@ simp) $ext105
		   ((mtimes simp)
		    (($basis simp array) $i) 
		    (($substitute simp) 
		     (($extsubb simp array) $i) $g)) )))  
		((msetq simp) $ext102 $false)) t $false))) 
	    ((mcond simp) $ext102 
	     ((mreturn simp) 
	      (($expand simp) ((mtimes simp) $f $g))) 
	     t ((mreturn simp) (($expand simp) $ext101))))) 
	  mexpr)
 
(add2lnc (quote (($@) $f $g)) $functions) 
(defprop %@ $@ noun) 
;;start definition with clifford operator
(defprop $& %& verb) 
(defprop $& "&" op) 
(putopr "&" '$\&)
(add2lnc (quote "&") $props) 
(defprop $& dimension-infix dimension) 
(defprop $& (32 38 32) dissym) 
(defprop $& 140 lbp) 
(defprop $& 180 rbp) 
(defprop $& parse-infix led) 
(defprop $& msize-infix grind) 
(defprop %& dimension-infix dimension) 
(defprop %& (32 38 32) dissym) 
;;clifford operator &
(mdefprop $& ((lambda) 
	      ((mlist) $f $g) 
	      ((mprog simp)
	       ((mlist simp) $i $j $ext101 $ext102 $ext103 $ext104 $ext105)
	       ((msetq simp) $ext101 0) 
	       ((msetq simp) $ext102 $true) 
	       ((msetq simp) $ext103 
		(($expand simp) $f)) 
	       ((mdo simp) $i $dim -1 nil 1 nil 
		((mprogn simp) ((msetq simp) $ext104 
				(($expand simp) 
				 (($bothcoef simp) $ext103 
				  (($basis simp array) $i))))
		 ((msetq simp) $ext105 (($first simp) $ext104)) 
		 ((mcond simp) ((mnotequal simp) $ext105 0) 
		  ((mprogn simp) ((msetq simp) $ext103 (($last simp) $ext104))
		   ((msetq simp) $ext101 ((mplus simp) $ext101 
;;(($& simp) $ext105				  
					  (($& simp) $ext105
					   ((mtimes simp)  
					    (($basis simp array) $i) 
					    (($substitute simp) 
					     (($extsubb2 simp array) $i) $g))
					   )))
		   ((msetq simp) $ext102 $false)) t $false))) 
	       ((mcond simp) $ext102 
		((mreturn simp) (($expand simp) 
				 ((mtimes simp) $f $g))) t 
				 ((mreturn simp) (($expand simp) $ext101))))) mexpr) 

(add2lnc (quote (($&) $f $g)) $functions) 
(defprop %& $& noun)

;;exterior differntial operator
(mdefprop $d ((lambda) 
	      ((mlist) $f) 
	      (($sum simp) 
	       (($@ simp) 
		(($basis simp array) $i) 
		(($diff simp) $f 
		 (($coords simp array) $i)))
	       $i 1 $dim)) mexpr) 

(add2lnc (quote (($d2) $f)) $functions)

;;clifford differntial operator
(mdefprop $d_c ((lambda) 
	      ((mlist) $f) 
	      (($sum simp) 
	       (($& simp) 
		(($basis simp array) $i) 
		(($diff simp) $f 
		 (($coords simp array) $i)))
	       $i 1 $dim)) mexpr)

(add2lnc (quote (($d_c) $f)) $functions)

;;another clifford operator with different basis.
;;clifford operator & and exterior operator go with.
;;but we ofen need to calc clifford algebra another basis at the same time.
;;before using &2 operator you must "infix("&2")$ "
(defprop $&2 %&2 verb) 
(defprop $&2 "&2" op) 
(putopr "&2" '$\&2)
(add2lnc (quote "&2") $props) 
(defprop $&2 dimension-infix dimension) 
(defprop $&2 (32 38 32) dissym) 
(defprop $&2 140 lbp) 
(defprop $&2 180 rbp) 
(defprop $&2 parse-infix led) 
(defprop $&2 msize-infix grind) 
(defprop %&2 dimension-infix dimension) 
(defprop %&2 (32 38 32) dissym) 
;;clifford operator &
(mdefprop $&2 ((lambda) 
	      ((mlist) $f $g) 
	      ((mprog simp)
	       ((mlist simp) $i $j $ext101 $ext102 $ext103 $ext104 $ext105)
	       ((msetq simp) $ext101 0) 
	       ((msetq simp) $ext102 $true) 
	       ((msetq simp) $ext103 
		(($expand simp) $f)) 
	       ((mdo simp) $i $n_dim -1 nil 1 nil 
		((mprogn simp) ((msetq simp) $ext104 
				(($expand simp) 
				 (($bothcoef simp) $ext103 
				  (($basis2 simp array) $i))))
		 ((msetq simp) $ext105 (($first simp) $ext104)) 
		 ((mcond simp) ((mnotequal simp) $ext105 0) 
		  ((mprogn simp) ((msetq simp) $ext103 (($last simp) $ext104))
		   ((msetq simp) $ext101 ((mplus simp) $ext101 
					  (($&2 simp) $ext105 ((mtimes simp) 
							      (($basis2 simp array) $i) 
							      (($substitute simp) 
							       (($extsubb4 simp array) $i) $g)))))
		   ((msetq simp) $ext102 $false)) t $false))) 
	       ((mcond simp) $ext102 
		((mreturn simp) (($expand simp) 
				 ((mtimes simp) $f $g))) t 
				 ((mreturn simp) (($expand simp) $ext101))))) mexpr) 

(add2lnc (quote (($&2) $f $g)) $functions) 
(defprop %&2 $&2 noun)
 

 











