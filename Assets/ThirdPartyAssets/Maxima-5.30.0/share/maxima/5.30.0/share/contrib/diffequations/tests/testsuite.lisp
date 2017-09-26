;;; The Maxima test suite is defined to be the sum of all the tests
;;; in the files listed in *testsuite-files*.
;;; Each entry can either be a plain file name (minus extension), e.g.,
;;; "testfile.mac", or a list containing a file name followed by the numbers
;;; of the test problems that are expected to fail, e.g.
;;; ("testfile.mac" 7 9 13).

;; The expected failures are due to testsuite issues.  The "wrong" answers 
;; are correct but I can't get the testsuite to agree.
;;
;; Errors in rtestode_kamke_1_1.mac, problems (33 73 191 192) and 
;; rtest_ode1_riccati.mac, problems (132 138 139) are due to Bessel function changes
;; around the beginning of 2008.

(setf $testsuite_files
      '( (mlist simp)
         "rtestode_murphy_1_1.mac"
	 "rtestode_murphy_1_2.mac"
         "rtestode_murphy_1_3.mac"
         "rtestode_murphy_1_4.mac"
         "rtestode_murphy_1_5.mac"
         "rtestode_murphy_1_6.mac"
         ((mlist) "rtestode_murphy_2_1.mac" 140 142) ; OK - testsuite issues
         "rtestode_murphy_2_2.mac"
         "rtestode_murphy_2_3.mac"
         "rtestode_murphy_2_4.mac"
         "rtestode_murphy_2_5.mac"
         ((mlist) "rtestode_kamke_1_1.mac" 34 75)
         ((mlist) "rtestode_kamke_1_2.mac" 9)  ; OK - testsuite issues
         "rtestode_kamke_1_3.mac"
         "rtestode_kamke_1_4.mac" 
         "rtestode_kamke_1_5.mac"
         ((mlist) "rtestode_kamke_1_6.mac" 20) ; OK - testsuite issues
         ((mlist) "rtestode_kamke_2_1.mac" 77) ; OK - testsuite issues
         ((mlist) "rtestode_kamke_2_2.mac" 132 ) ; OK - testsuite issues
         ((mlist) "rtestode_kamke_2_3.mac" 83 107 181) ; OK - testsuite issues
         "rtestode_kamke_2_4.mac"
         "rtestode_kamke_2_5.mac"
	 "rtest_sym.mac" 
	 "rtest_sym2.mac"
	 ((mlist) "rtest_ode1_riccati.mac" 138) ; OK - testsuite issues
         "rtest_ode1_abel.mac"
         "rtestode_odelin.mac"
         "rtestode_utils.mac"
	)
)
