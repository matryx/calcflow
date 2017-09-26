;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.221 2010/05/26 19:25:52 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.204 2010/02/23 05:21:30 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.114 2010/05/17 01:42:14 rtoy Exp $")

;;; Using Lisp CMU Common Lisp CVS Head 2010-05-25 18:21:07 (20A Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :colnew)


(labels ((multi-entry-colnew
             (%name% ncomp m aleft aright zeta ipar ltol tol fixpnt ispace
              fspace iflag fsub dfsub gsub dgsub guess)
           (declare (type (array double-float (*)) fspace fixpnt tol zeta)
                    (type double-float aright aleft)
                    (type (array f2cl-lib:integer4 (*)) ispace ltol ipar m)
                    (type (f2cl-lib:integer4) iflag ncomp))
           (let ((colloc-rho
                  (make-array 7
                              :element-type 'double-float
                              :displaced-to (colloc-part-0
                                             *colloc-common-block*)
                              :displaced-index-offset 0))
                 (colloc-coef
                  (make-array 49
                              :element-type 'double-float
                              :displaced-to (colloc-part-0
                                             *colloc-common-block*)
                              :displaced-index-offset 7))
                 (colord-mt
                  (make-array 20
                              :element-type 'f2cl-lib:integer4
                              :displaced-to (colord-part-0
                                             *colord-common-block*)
                              :displaced-index-offset 5))
                 (colsid-tzeta
                  (make-array 40
                              :element-type 'double-float
                              :displaced-to (colsid-part-0
                                             *colsid-common-block*)
                              :displaced-index-offset 0))
                 (colest-tolin
                  (make-array 40
                              :element-type 'double-float
                              :displaced-to (colest-part-0
                                             *colest-common-block*)
                              :displaced-index-offset 120))
                 (colest-lttol
                  (make-array 40
                              :element-type 'f2cl-lib:integer4
                              :displaced-to (colest-part-1
                                             *colest-common-block*)
                              :displaced-index-offset 40)))
             (symbol-macrolet ((precis
                                (aref (colout-part-0 *colout-common-block*) 0))
                               (iout
                                (aref (colout-part-1 *colout-common-block*) 0))
                               (iprint
                                (aref (colout-part-1 *colout-common-block*) 1))
                               (rho colloc-rho)
                               (coef colloc-coef)
                               (k
                                (aref (colord-part-0 *colord-common-block*) 0))
                               (nc
                                (aref (colord-part-0 *colord-common-block*) 1))
                               (mstar
                                (aref (colord-part-0 *colord-common-block*) 2))
                               (kd
                                (aref (colord-part-0 *colord-common-block*) 3))
                               (mmax
                                (aref (colord-part-0 *colord-common-block*) 4))
                               (mt colord-mt)
                               (n
                                (aref (colapr-part-0 *colapr-common-block*) 0))
                               (nold
                                (aref (colapr-part-0 *colapr-common-block*) 1))
                               (nmax
                                (aref (colapr-part-0 *colapr-common-block*) 2))
                               (nz
                                (aref (colapr-part-0 *colapr-common-block*) 3))
                               (ndmz
                                (aref (colapr-part-0 *colapr-common-block*) 4))
                               (mshflg
                                (aref (colmsh-part-0 *colmsh-common-block*) 0))
                               (mshnum
                                (aref (colmsh-part-0 *colmsh-common-block*) 1))
                               (mshlmt
                                (aref (colmsh-part-0 *colmsh-common-block*) 2))
                               (mshalt
                                (aref (colmsh-part-0 *colmsh-common-block*) 3))
                               (tzeta colsid-tzeta)
                               (tleft
                                (aref (colsid-part-0 *colsid-common-block*)
                                      40))
                               (tright
                                (aref (colsid-part-0 *colsid-common-block*)
                                      41))
                               (nonlin
                                (aref (colnln-part-0 *colnln-common-block*) 0))
                               (limit
                                (aref (colnln-part-0 *colnln-common-block*) 2))
                               (icare
                                (aref (colnln-part-0 *colnln-common-block*) 3))
                               (iguess
                                (aref (colnln-part-0 *colnln-common-block*) 4))
                               (tolin colest-tolin)
                               (lttol colest-lttol)
                               (ntol
                                (aref (colest-part-1 *colest-common-block*)
                                      80)))
               (f2cl-lib:with-multi-array-data
                   ((m f2cl-lib:integer4 m-%data% m-%offset%)
                    (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%)
                    (ltol f2cl-lib:integer4 ltol-%data% ltol-%offset%)
                    (ispace f2cl-lib:integer4 ispace-%data% ispace-%offset%)
                    (zeta double-float zeta-%data% zeta-%offset%)
                    (tol double-float tol-%data% tol-%offset%)
                    (fixpnt double-float fixpnt-%data% fixpnt-%offset%)
                    (fspace double-float fspace-%data% fspace-%offset%))
                 (prog ((ic 0) (k2 0) (idmz 0) (np1 0) (linteg 0) (lpvtw 0)
                        (lpvtg 0) (ldscl 0) (lscl 0) (laccum 0) (lslope 0)
                        (lvalst 0) (lrhs 0) (ldqdmz 0) (ldqz 0) (ldeldz 0)
                        (ldelz 0) (ldmz 0) (lz 0) (lv 0) (lw 0) (lxiold 0)
                        (lg 0) (lxi 0) (nmaxi 0) (nmaxf 0) (nsizef 0) (nfixf 0)
                        (nsizei 0) (nfixi 0) (ib 0) (nrec 0) (ip 0) (nfxpnt 0)
                        (ndimi 0) (ndimf 0) (iread 0) (i 0) (precp1 0.0)
                        (dummy (make-array 1 :element-type 'double-float)))
                   (declare (type (array double-float (1)) dummy)
                            (type double-float precp1)
                            (type (f2cl-lib:integer4) i iread ndimf ndimi
                                                      nfxpnt ip nrec ib nfixi
                                                      nsizei nfixf nsizef nmaxf
                                                      nmaxi lxi lg lxiold lw lv
                                                      lz ldmz ldelz ldeldz ldqz
                                                      ldqdmz lrhs lvalst lslope
                                                      laccum lscl ldscl lpvtg
                                                      lpvtw linteg np1 idmz k2
                                                      ic))
                   (if (eq %name% 'colsys) (go colsys))
                  colsys
                   (if
                    (<= (f2cl-lib:fref ipar-%data% (7) ((1 1)) ipar-%offset%)
                        0)
                    (f2cl-lib:fformat 6
                                      ("~%" "~%"
                                       " VERSION *COLNEW* OF COLSYS .    " "~%"
                                       "~%" "~%")))
                   (setf iout 6)
                   (setf precis 1.0)
                  label10
                   (setf precis (/ precis 2.0))
                   (setf precp1 (+ precis 1.0))
                   (if (> precp1 1.0) (go label10))
                   (setf precis (* precis 100.0))
                   (setf iflag -3)
                   (if (or (< ncomp 1) (> ncomp 20)) (go end_label))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i ncomp) nil)
                     (tagbody
                       (if
                        (or
                         (< (f2cl-lib:fref m-%data% (i) ((1 1)) m-%offset%) 1)
                         (> (f2cl-lib:fref m-%data% (i) ((1 1)) m-%offset%) 4))
                        (go end_label))
                      label20))
                   (setf nonlin
                           (f2cl-lib:fref ipar-%data%
                                          (1)
                                          ((1 1))
                                          ipar-%offset%))
                   (setf k
                           (f2cl-lib:fref ipar-%data%
                                          (2)
                                          ((1 1))
                                          ipar-%offset%))
                   (setf n
                           (f2cl-lib:fref ipar-%data%
                                          (3)
                                          ((1 1))
                                          ipar-%offset%))
                   (if (= n 0) (setf n 5))
                   (setf iread
                           (f2cl-lib:fref ipar-%data%
                                          (8)
                                          ((1 1))
                                          ipar-%offset%))
                   (setf iguess
                           (f2cl-lib:fref ipar-%data%
                                          (9)
                                          ((1 1))
                                          ipar-%offset%))
                   (if (and (= nonlin 0) (= iguess 1)) (setf iguess 0))
                   (if (and (>= iguess 2) (= iread 0)) (setf iread 1))
                   (setf icare
                           (f2cl-lib:fref ipar-%data%
                                          (10)
                                          ((1 1))
                                          ipar-%offset%))
                   (setf ntol
                           (f2cl-lib:fref ipar-%data%
                                          (4)
                                          ((1 1))
                                          ipar-%offset%))
                   (setf ndimf
                           (f2cl-lib:fref ipar-%data%
                                          (5)
                                          ((1 1))
                                          ipar-%offset%))
                   (setf ndimi
                           (f2cl-lib:fref ipar-%data%
                                          (6)
                                          ((1 1))
                                          ipar-%offset%))
                   (setf nfxpnt
                           (f2cl-lib:fref ipar-%data%
                                          (11)
                                          ((1 1))
                                          ipar-%offset%))
                   (setf iprint
                           (f2cl-lib:fref ipar-%data%
                                          (7)
                                          ((1 1))
                                          ipar-%offset%))
                   (setf mstar 0)
                   (setf mmax 0)
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i ncomp) nil)
                     (tagbody
                       (setf mmax
                               (f2cl-lib:max0 mmax
                                              (f2cl-lib:fref m-%data%
                                                             (i)
                                                             ((1 1))
                                                             m-%offset%)))
                       (setf mstar
                               (f2cl-lib:int-add mstar
                                                 (f2cl-lib:fref m-%data%
                                                                (i)
                                                                ((1 1))
                                                                m-%offset%)))
                       (setf (f2cl-lib:fref mt (i) ((1 20)))
                               (f2cl-lib:fref m-%data% (i) ((1 1)) m-%offset%))
                      label30))
                   (if (= k 0)
                       (setf k
                               (f2cl-lib:max0 (f2cl-lib:int-add mmax 1)
                                              (f2cl-lib:int-sub 5 mmax))))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i mstar) nil)
                     (tagbody
                      label40
                       (setf (f2cl-lib:fref tzeta (i) ((1 40)))
                               (f2cl-lib:fref zeta-%data%
                                              (i)
                                              ((1 1))
                                              zeta-%offset%))))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i ntol) nil)
                     (tagbody
                       (setf (f2cl-lib:fref lttol (i) ((1 40)))
                               (f2cl-lib:fref ltol-%data%
                                              (i)
                                              ((1 1))
                                              ltol-%offset%))
                      label50
                       (setf (f2cl-lib:fref tolin (i) ((1 40)))
                               (f2cl-lib:fref tol-%data%
                                              (i)
                                              ((1 1))
                                              tol-%offset%))))
                   (setf tleft aleft)
                   (setf tright aright)
                   (setf nc ncomp)
                   (setf kd (f2cl-lib:int-mul k ncomp))
                   (if (> iprint -1) (go label80))
                   (if (> nonlin 0) (go label60))
                   (f2cl-lib:fformat iout
                                     ("~%" "~%" "~%"
                                      " THE NUMBER OF (LINEAR) DIFF EQNS IS " 1
                                      (("~3D")) "~%" "~1@T" "THEIR ORDERS ARE"
                                      20 (("~3D")) "~%")
                                     ncomp
                                     (do ((ip 1 (f2cl-lib:int-add ip 1))
                                          (%ret nil))
                                         ((> ip ncomp) (nreverse %ret))
                                       (declare (type f2cl-lib:integer4 ip))
                                       (push
                                        (f2cl-lib:fref m-%data%
                                                       (ip)
                                                       ((1 1))
                                                       m-%offset%)
                                        %ret)))
                   (go label70)
                  label60
                   (f2cl-lib:fformat iout
                                     ("~%" "~%" "~%"
                                      " THE NUMBER OF (NONLINEAR) DIFF EQNS IS "
                                      1 (("~3D")) "~%" "~1@T"
                                      "THEIR ORDERS ARE" 20 (("~3D")) "~%")
                                     ncomp
                                     (do ((ip 1 (f2cl-lib:int-add ip 1))
                                          (%ret nil))
                                         ((> ip ncomp) (nreverse %ret))
                                       (declare (type f2cl-lib:integer4 ip))
                                       (push
                                        (f2cl-lib:fref m-%data%
                                                       (ip)
                                                       ((1 1))
                                                       m-%offset%)
                                        %ret)))
                  label70
                   (f2cl-lib:fformat iout
                                     (" SIDE CONDITION POINTS ZETA" 8
                                      (("~10,6,0,'*,F")) 4
                                      ("~%" "~27@T" 8 (("~10,6,0,'*,F"))) "~%")
                                     (do ((ip 1 (f2cl-lib:int-add ip 1))
                                          (%ret nil))
                                         ((> ip mstar) (nreverse %ret))
                                       (declare (type f2cl-lib:integer4 ip))
                                       (push
                                        (f2cl-lib:fref zeta-%data%
                                                       (ip)
                                                       ((1 1))
                                                       zeta-%offset%)
                                        %ret)))
                   (if (> nfxpnt 0)
                       (f2cl-lib:fformat iout
                                         (" THERE ARE" 1 (("~5D"))
                                          " FIXED POINTS IN THE MESH -" 10
                                          (6 (("~10,6,0,'*,F")) "~%") "~%")
                                         nfxpnt
                                         (do ((ip 1 (f2cl-lib:int-add ip 1))
                                              (%ret nil))
                                             ((> ip nfxpnt) (nreverse %ret))
                                           (declare (type f2cl-lib:integer4 ip))
                                           (push
                                            (f2cl-lib:fref fixpnt-%data%
                                                           (ip)
                                                           ((1 1))
                                                           fixpnt-%offset%)
                                            %ret))))
                   (f2cl-lib:fformat iout
                                     (" NUMBER OF COLLOC PTS PER INTERVAL IS" 1
                                      (("~3D")) "~%")
                                     k)
                   (f2cl-lib:fformat iout
                                     (" COMPONENTS OF Z REQUIRING TOLERANCES -"
                                      8 ("~7@T" 1 (("~2D")) "~1@T") 4
                                      ("~%" "~38@T" 8 (("~10D"))) "~%")
                                     (do ((ip 1 (f2cl-lib:int-add ip 1))
                                          (%ret nil))
                                         ((> ip ntol) (nreverse %ret))
                                       (declare (type f2cl-lib:integer4 ip))
                                       (push
                                        (f2cl-lib:fref ltol-%data%
                                                       (ip)
                                                       ((1 1))
                                                       ltol-%offset%)
                                        %ret)))
                   (f2cl-lib:fformat iout
                                     (" CORRESPONDING ERROR TOLERANCES -"
                                      "~6@T" 8 (("~10,2,2,0,'*,,'DE")) 4
                                      ("~%" "~39@T" 8 (("~10,2,2,0,'*,,'DE")))
                                      "~%")
                                     (do ((ip 1 (f2cl-lib:int-add ip 1))
                                          (%ret nil))
                                         ((> ip ntol) (nreverse %ret))
                                       (declare (type f2cl-lib:integer4 ip))
                                       (push
                                        (f2cl-lib:fref tol-%data%
                                                       (ip)
                                                       ((1 1))
                                                       tol-%offset%)
                                        %ret)))
                   (if (>= iguess 2)
                       (f2cl-lib:fformat iout
                                         (" INITIAL MESH(ES) AND Z,DMZ PROVIDED BY USER"
                                          "~%")))
                   (if (= iread 2)
                       (f2cl-lib:fformat iout
                                         (" NO ADAPTIVE MESH SELECTION" "~%")))
                  label80
                   (if (or (< k 0) (> k 7)) (go end_label))
                   (if (< n 0) (go end_label))
                   (if (or (< iread 0) (> iread 2)) (go end_label))
                   (if (or (< iguess 0) (> iguess 4)) (go end_label))
                   (if (or (< icare 0) (> icare 2)) (go end_label))
                   (if (or (< ntol 0) (> ntol mstar)) (go end_label))
                   (if (< nfxpnt 0) (go end_label))
                   (if (or (< iprint -1) (> iprint 1)) (go end_label))
                   (if (or (< mstar 0) (> mstar 40)) (go end_label))
                   (setf ip 1)
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i mstar) nil)
                     (tagbody
                       (if
                        (or
                         (<
                          (f2cl-lib:dabs
                           (-
                            (f2cl-lib:fref zeta-%data%
                                           (i)
                                           ((1 1))
                                           zeta-%offset%)
                            aleft))
                          precis)
                         (<
                          (f2cl-lib:dabs
                           (-
                            (f2cl-lib:fref zeta-%data%
                                           (i)
                                           ((1 1))
                                           zeta-%offset%)
                            aright))
                          precis))
                        (go label100))
                      label90
                       (if (> ip nfxpnt) (go end_label))
                       (if
                        (<
                         (-
                          (f2cl-lib:fref zeta-%data% (i) ((1 1)) zeta-%offset%)
                          precis)
                         (f2cl-lib:fref fixpnt-%data%
                                        (ip)
                                        ((1 1))
                                        fixpnt-%offset%))
                        (go label95))
                       (setf ip (f2cl-lib:int-add ip 1))
                       (go label90)
                      label95
                       (if
                        (<
                         (+
                          (f2cl-lib:fref zeta-%data% (i) ((1 1)) zeta-%offset%)
                          precis)
                         (f2cl-lib:fref fixpnt-%data%
                                        (ip)
                                        ((1 1))
                                        fixpnt-%offset%))
                        (go end_label))
                      label100))
                   (setf mshlmt 3)
                   (setf mshflg 0)
                   (setf mshnum 1)
                   (setf mshalt 1)
                   (setf limit 40)
                   (setf nrec 0)
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i mstar) nil)
                     (tagbody
                       (setf ib
                               (f2cl-lib:int-sub (f2cl-lib:int-add mstar 1) i))
                       (if
                        (>=
                         (f2cl-lib:fref zeta-%data% (ib) ((1 1)) zeta-%offset%)
                         aright)
                        (setf nrec i))
                      label110))
                   (setf nfixi mstar)
                   (setf nsizei (f2cl-lib:int-add 3 kd mstar))
                   (setf nfixf
                           (f2cl-lib:int-add
                            (f2cl-lib:int-mul nrec (f2cl-lib:int-mul 2 mstar))
                            (f2cl-lib:int-mul 5 mstar)
                            3))
                   (setf nsizef
                           (f2cl-lib:int-add 4
                                             (f2cl-lib:int-mul 3 mstar)
                                             (f2cl-lib:int-mul
                                              (f2cl-lib:int-add kd 5)
                                              (f2cl-lib:int-add kd mstar))
                                             (f2cl-lib:int-mul
                                              (f2cl-lib:int-sub
                                               (f2cl-lib:int-mul 2 mstar)
                                               nrec)
                                              2
                                              mstar)))
                   (setf nmaxf
                           (the f2cl-lib:integer4
                                (truncate (- ndimf nfixf) nsizef)))
                   (setf nmaxi
                           (the f2cl-lib:integer4
                                (truncate (- ndimi nfixi) nsizei)))
                   (if (< iprint 1)
                       (f2cl-lib:fformat iout
                                         (" THE MAXIMUM NUMBER OF SUBINTERVALS IS MIN ("
                                          1 (("~4D")) " (ALLOWED FROM FSPACE),"
                                          1 (("~4D"))
                                          " (ALLOWED FROM ISPACE) )" "~%")
                                         nmaxf
                                         nmaxi))
                   (setf nmax (f2cl-lib:min0 nmaxf nmaxi))
                   (if (< nmax n) (go end_label))
                   (if (< nmax (f2cl-lib:int-add nfxpnt 1)) (go end_label))
                   (if
                    (and
                     (< nmax (f2cl-lib:int-add (f2cl-lib:int-mul 2 nfxpnt) 2))
                     (< iprint 1))
                    (f2cl-lib:fformat iout
                                      ("~%"
                                       " INSUFFICIENT SPACE TO DOUBLE MESH FOR ERROR ESTIMATE"
                                       "~%")))
                   (setf lxi 1)
                   (setf lg (f2cl-lib:int-add lxi nmax 1))
                   (setf lxiold
                           (f2cl-lib:int-add lg
                                             (f2cl-lib:int-mul 2
                                                               mstar
                                                               (f2cl-lib:int-add
                                                                (f2cl-lib:int-mul
                                                                 nmax
                                                                 (f2cl-lib:int-sub
                                                                  (f2cl-lib:int-mul
                                                                   2
                                                                   mstar)
                                                                  nrec))
                                                                nrec))))
                   (setf lw (f2cl-lib:int-add lxiold nmax 1))
                   (setf lv
                           (f2cl-lib:int-add lw
                                             (f2cl-lib:int-mul (expt kd 2)
                                                               nmax)))
                   (setf lz
                           (f2cl-lib:int-add lv
                                             (f2cl-lib:int-mul mstar kd nmax)))
                   (setf ldmz
                           (f2cl-lib:int-add lz
                                             (f2cl-lib:int-mul mstar
                                                               (f2cl-lib:int-add
                                                                nmax
                                                                1))))
                   (setf ldelz
                           (f2cl-lib:int-add ldmz (f2cl-lib:int-mul kd nmax)))
                   (setf ldeldz
                           (f2cl-lib:int-add ldelz
                                             (f2cl-lib:int-mul mstar
                                                               (f2cl-lib:int-add
                                                                nmax
                                                                1))))
                   (setf ldqz
                           (f2cl-lib:int-add ldeldz
                                             (f2cl-lib:int-mul kd nmax)))
                   (setf ldqdmz
                           (f2cl-lib:int-add ldqz
                                             (f2cl-lib:int-mul mstar
                                                               (f2cl-lib:int-add
                                                                nmax
                                                                1))))
                   (setf lrhs
                           (f2cl-lib:int-add ldqdmz
                                             (f2cl-lib:int-mul kd nmax)))
                   (setf lvalst
                           (f2cl-lib:int-add lrhs
                                             (f2cl-lib:int-mul kd nmax)
                                             mstar))
                   (setf lslope
                           (f2cl-lib:int-add lvalst
                                             (f2cl-lib:int-mul 4 mstar nmax)))
                   (setf laccum (f2cl-lib:int-add lslope nmax))
                   (setf lscl (f2cl-lib:int-add laccum nmax 1))
                   (setf ldscl
                           (f2cl-lib:int-add lscl
                                             (f2cl-lib:int-mul mstar
                                                               (f2cl-lib:int-add
                                                                nmax
                                                                1))))
                   (setf lpvtg 1)
                   (setf lpvtw
                           (f2cl-lib:int-add lpvtg
                                             (f2cl-lib:int-mul mstar
                                                               (f2cl-lib:int-add
                                                                nmax
                                                                1))))
                   (setf linteg
                           (f2cl-lib:int-add lpvtw (f2cl-lib:int-mul kd nmax)))
                   (if (< iguess 2) (go label160))
                   (setf nold n)
                   (if (= iguess 4)
                       (setf nold
                               (f2cl-lib:fref ispace-%data%
                                              (1)
                                              ((1 1))
                                              ispace-%offset%)))
                   (setf nz (f2cl-lib:int-mul mstar (f2cl-lib:int-add nold 1)))
                   (setf ndmz (f2cl-lib:int-mul kd nold))
                   (setf np1 (f2cl-lib:int-add n 1))
                   (if (= iguess 4) (setf np1 (f2cl-lib:int-add np1 nold 1)))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i nz) nil)
                     (tagbody
                      label120
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-sub
                                              (f2cl-lib:int-add lz i)
                                              1))
                                            ((1 1))
                                            fspace-%offset%)
                               (f2cl-lib:fref fspace-%data%
                                              ((f2cl-lib:int-add np1 i))
                                              ((1 1))
                                              fspace-%offset%))))
                   (setf idmz (f2cl-lib:int-add np1 nz))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i ndmz) nil)
                     (tagbody
                      label125
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-sub
                                              (f2cl-lib:int-add ldmz i)
                                              1))
                                            ((1 1))
                                            fspace-%offset%)
                               (f2cl-lib:fref fspace-%data%
                                              ((f2cl-lib:int-add idmz i))
                                              ((1 1))
                                              fspace-%offset%))))
                   (setf np1 (f2cl-lib:int-add nold 1))
                   (if (= iguess 4) (go label140))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i np1) nil)
                     (tagbody
                      label130
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-sub
                                              (f2cl-lib:int-add lxiold i)
                                              1))
                                            ((1 1))
                                            fspace-%offset%)
                               (f2cl-lib:fref fspace-%data%
                                              ((f2cl-lib:int-sub
                                                (f2cl-lib:int-add lxi i)
                                                1))
                                              ((1 1))
                                              fspace-%offset%))))
                   (go label160)
                  label140
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i np1) nil)
                     (tagbody
                      label150
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-sub
                                              (f2cl-lib:int-add lxiold i)
                                              1))
                                            ((1 1))
                                            fspace-%offset%)
                               (f2cl-lib:fref fspace-%data%
                                              ((f2cl-lib:int-add n 1 i))
                                              ((1 1))
                                              fspace-%offset%))))
                  label160
                   (consts k rho coef)
                   (newmsh (f2cl-lib:int-add 3 iread)
                    (f2cl-lib:array-slice fspace double-float (lxi) ((1 1)))
                    (f2cl-lib:array-slice fspace double-float (lxiold) ((1 1)))
                    dummy dummy dummy dummy dummy nfxpnt fixpnt)
                   (if (>= iguess 2) (go label230))
                   (setf np1 (f2cl-lib:int-add n 1))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i np1) nil)
                     (tagbody
                      label210
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-sub
                                              (f2cl-lib:int-add i lxiold)
                                              1))
                                            ((1 1))
                                            fspace-%offset%)
                               (f2cl-lib:fref fspace-%data%
                                              ((f2cl-lib:int-sub
                                                (f2cl-lib:int-add i lxi)
                                                1))
                                              ((1 1))
                                              fspace-%offset%))))
                   (setf nold n)
                   (if (or (= nonlin 0) (= iguess 1)) (go label230))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i nz) nil)
                     (tagbody
                      label220
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-add
                                              (f2cl-lib:int-sub lz 1)
                                              i))
                                            ((1 1))
                                            fspace-%offset%)
                               0.0)))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i ndmz) nil)
                     (tagbody
                      label225
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-add
                                              (f2cl-lib:int-sub ldmz 1)
                                              i))
                                            ((1 1))
                                            fspace-%offset%)
                               0.0)))
                  label230
                   (if (>= iguess 2) (setf iguess 0))
                   (multiple-value-bind
                         (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                          var-9 var-10 var-11 var-12 var-13 var-14 var-15
                          var-16 var-17 var-18 var-19 var-20 var-21 var-22
                          var-23 var-24 var-25 var-26 var-27)
                       (contrl
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (lxi)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (lxiold)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace double-float (lz) ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (ldmz)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (lrhs)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (ldelz)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (ldeldz)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (ldqz)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (ldqdmz)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace double-float (lg) ((1 1)))
                        (f2cl-lib:array-slice fspace double-float (lw) ((1 1)))
                        (f2cl-lib:array-slice fspace double-float (lv) ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (lvalst)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (lslope)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (lscl)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (ldscl)
                                              ((1 1)))
                        (f2cl-lib:array-slice fspace
                                              double-float
                                              (laccum)
                                              ((1 1)))
                        (f2cl-lib:array-slice ispace
                                              f2cl-lib:integer4
                                              (lpvtg)
                                              ((1 1)))
                        (f2cl-lib:array-slice ispace
                                              f2cl-lib:integer4
                                              (linteg)
                                              ((1 1)))
                        (f2cl-lib:array-slice ispace
                                              f2cl-lib:integer4
                                              (lpvtw)
                                              ((1 1)))
                        nfxpnt fixpnt iflag fsub dfsub gsub dgsub guess)
                     (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                      var-7 var-8 var-9 var-10 var-11 var-12
                                      var-13 var-14 var-15 var-16 var-17 var-18
                                      var-19 var-20 var-21 var-23 var-24 var-25
                                      var-26 var-27))
                     (setf iflag var-22))
                   (setf (f2cl-lib:fref ispace-%data%
                                        (1)
                                        ((1 1))
                                        ispace-%offset%)
                           n)
                   (setf (f2cl-lib:fref ispace-%data%
                                        (2)
                                        ((1 1))
                                        ispace-%offset%)
                           k)
                   (setf (f2cl-lib:fref ispace-%data%
                                        (3)
                                        ((1 1))
                                        ispace-%offset%)
                           ncomp)
                   (setf (f2cl-lib:fref ispace-%data%
                                        (4)
                                        ((1 1))
                                        ispace-%offset%)
                           mstar)
                   (setf (f2cl-lib:fref ispace-%data%
                                        (5)
                                        ((1 1))
                                        ispace-%offset%)
                           mmax)
                   (setf (f2cl-lib:fref ispace-%data%
                                        (6)
                                        ((1 1))
                                        ispace-%offset%)
                           (f2cl-lib:int-add nz ndmz n 2))
                   (setf k2 (f2cl-lib:int-mul k k))
                   (setf (f2cl-lib:fref ispace-%data%
                                        (7)
                                        ((1 1))
                                        ispace-%offset%)
                           (f2cl-lib:int-sub
                            (f2cl-lib:int-add
                             (f2cl-lib:fref ispace-%data%
                                            (6)
                                            ((1 1))
                                            ispace-%offset%)
                             k2)
                            1))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i ncomp) nil)
                     (tagbody
                      label240
                       (setf (f2cl-lib:fref ispace-%data%
                                            ((f2cl-lib:int-add 7 i))
                                            ((1 1))
                                            ispace-%offset%)
                               (f2cl-lib:fref m-%data%
                                              (i)
                                              ((1 1))
                                              m-%offset%))))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i nz) nil)
                     (tagbody
                      label250
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-add n 1 i))
                                            ((1 1))
                                            fspace-%offset%)
                               (f2cl-lib:fref fspace-%data%
                                              ((f2cl-lib:int-add
                                                (f2cl-lib:int-sub lz 1)
                                                i))
                                              ((1 1))
                                              fspace-%offset%))))
                   (setf idmz (f2cl-lib:int-add n 1 nz))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i ndmz) nil)
                     (tagbody
                      label255
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-add idmz i))
                                            ((1 1))
                                            fspace-%offset%)
                               (f2cl-lib:fref fspace-%data%
                                              ((f2cl-lib:int-add
                                                (f2cl-lib:int-sub ldmz 1)
                                                i))
                                              ((1 1))
                                              fspace-%offset%))))
                   (setf ic (f2cl-lib:int-add idmz ndmz))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i k2) nil)
                     (tagbody
                      label258
                       (setf (f2cl-lib:fref fspace-%data%
                                            ((f2cl-lib:int-add ic i))
                                            ((1 1))
                                            fspace-%offset%)
                               (f2cl-lib:fref coef (i) ((1 49))))))
                   (go end_label)
                  end_label
                   (return
                    (values nil
                            nil
                            nil
                            nil
                            nil
                            nil
                            nil
                            nil
                            nil
                            nil
                            nil
                            iflag
                            nil
                            nil
                            nil
                            nil
                            nil))))))))
  (defun colnew
         (ncomp m aleft aright zeta ipar ltol tol fixpnt ispace fspace iflag
          fsub dfsub gsub dgsub guess)
    (multiple-value-bind
          (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
        (multi-entry-colnew 'colnew ncomp m aleft aright zeta ipar ltol tol
         fixpnt ispace fspace iflag fsub dfsub gsub dgsub guess)
      (values v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)))
  (defun colsys
         (ncomp m aleft aright zeta ipar ltol tol fixpnt ispace fspace iflag
          fsub dfsub gsub dgsub guess)
    (multiple-value-bind
          (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
        (multi-entry-colnew 'colsys ncomp m aleft aright zeta ipar ltol tol
         fixpnt ispace fspace iflag fsub dfsub gsub dgsub guess)
      (values v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::colnew
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (1)) double-float
                        double-float (array double-float (1))
                        (array fortran-to-lisp::integer4 (1))
                        (array fortran-to-lisp::integer4 (1))
                        (array double-float (1)) (array double-float (1))
                        (array fortran-to-lisp::integer4 (1))
                        (array double-float (1)) (fortran-to-lisp::integer4) t
                        t t t t)
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::iflag nil nil nil nil nil)
           :calls '(fortran-to-lisp::contrl fortran-to-lisp::newmsh
                    fortran-to-lisp::consts))))

