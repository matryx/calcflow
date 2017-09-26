(cond (($get '$cartan '$version) (merror "CARTAN already loaded"))
      (t ($put '$cartan '$v20041209 '$version))
)


; (SETQ SAVENO 2550) 
(DEFPROP $\| %\| VERB) 
(DEFPROP $\| "|" OP) 
(putopr "|" '$\|)
(ADD2LNC (QUOTE "|") $PROPS) 
(DEFPROP $\| DIMENSION-infix DIMENSION) 
(DEFPROP $\| (#\Space #\| #\Space) DISSYM) 
(DEFPROP $\| 120 LBP) 
(DEFPROP $\| 180 RBP) 
(DEFPROP $\| PARSE-INFIX LED) 
(DEFPROP $\| MSIZE-INFIX GRIND) 
(DEFPROP %\| DIMENSION-infix DIMENSION) 
(DEFPROP %\| (#\Space #\| #\Space) DISSYM) 
(MDEFPROP $\| ((LAMBDA) ((MLIST) $V $F) ((MPROG SIMP) ((MLIST SIMP)
 $I $J $EXT101 $EXT102 $EXT103 $EXT104) ((MSETQ SIMP) $EXT103 
(($EXPAND SIMP) $F)) ((MSETQ SIMP) $EXT102 ((MTIMES SIMP) 
(($V SIMP ARRAY) 1) (($COEFF SIMP) $EXT103 (($CARTAN_BASIS SIMP ARRAY) 1)))) 
((MDO SIMP) $I 2 NIL NIL $CARTAN_DIM NIL ((MPROGN SIMP) ((MSETQ SIMP) 
$EXT101 (($COEFF SIMP) $EXT103 (($CARTAN_BASIS SIMP ARRAY) $I))) 
((MCOND SIMP) ((MNOTEQUAL SIMP) $EXT101 0) ((MSETQ SIMP) $EXT101
 (($SUBSTITUTE SIMP) (($EXTSUB SIMP ARRAY) $I) $EXT101)) T $FALSE)
 ((MSETQ SIMP) $EXT102 ((MPLUS SIMP) $EXT102 ((MTIMES SIMP) $EXT101
 (($V SIMP ARRAY) $I)))))) ((MRETURN SIMP) (($EXPAND SIMP) $EXT102))))
 MEXPR) 
(ADD2LNC (QUOTE (($\|) $V $F)) $FUNCTIONS) 
(DEFPROP %\| $\| NOUN) 
(DEFPROP $~ %~ VERB) 
(DEFPROP $~ "~" OP) 
(putopr "~" '$~)
(ADD2LNC (QUOTE "~") $PROPS) 
(DEFPROP $~ DIMENSION-infix DIMENSION) 
(DEFPROP $~ (#\Space #\~ #\Space) DISSYM) 
(DEFPROP $~ 140 LBP) 
(DEFPROP $~ 180 RBP) 
(DEFPROP $~ PARSE-INFIX LED) 
(DEFPROP $~ MSIZE-INFIX GRIND) 
(DEFPROP %~ DIMENSION-infix DIMENSION) 
(DEFPROP %~ (#\Space #\~ #\Space) DISSYM) 
(MDEFPROP $~ ((LAMBDA) ((MLIST) $F $G) ((MPROG SIMP) ((MLIST SIMP) $I $J $EXT101 $EXT102 $EXT103 $EXT104 $EXT105) ((MSETQ SIMP) $EXT101 0) ((MSETQ SIMP) $EXT102 $TRUE) ((MSETQ SIMP) $EXT103 (($EXPAND SIMP) $F)) ((MDO SIMP) $I $CARTAN_DIM -1 NIL 1 NIL ((MPROGN SIMP) ((MSETQ SIMP) $EXT104 (($EXPAND SIMP) (($BOTHCOEF SIMP) $EXT103 (($CARTAN_BASIS SIMP ARRAY) $I)))) ((MSETQ SIMP) $EXT105 (($FIRST SIMP) $EXT104)) ((MCOND SIMP) ((MNOTEQUAL SIMP) $EXT105 0) ((MPROGN SIMP) ((MSETQ SIMP) $EXT103 (($LAST SIMP) $EXT104)) ((MSETQ SIMP) $CARTAN_DIM ((MPLUS SIMP) -1 $I)) ((MSETQ SIMP) $EXT101 ((MPLUS SIMP) $EXT101 (($~ SIMP) $EXT105 ((MTIMES SIMP) (($CARTAN_BASIS SIMP ARRAY) $I) (($SUBSTITUTE SIMP) (($EXTSUBB SIMP ARRAY) $I) $G))))) ((MSETQ SIMP) $CARTAN_DIM $EXTDIM) ((MSETQ SIMP) $EXT102 $FALSE)) T $FALSE))) ((MCOND SIMP) $EXT102 ((MRETURN SIMP) (($EXPAND SIMP) ((MTIMES SIMP) $F $G))) T ((MRETURN SIMP) (($EXPAND SIMP) $EXT101))))) MEXPR) 
(ADD2LNC (QUOTE (($~) $F $G)) $FUNCTIONS) 
(DEFPROP %~ $~ NOUN) 
(MDEFPROP $EXT_DIFF ((LAMBDA) ((MLIST) $F) (($SUM SIMP) (($~ SIMP) 
(($CARTAN_BASIS SIMP ARRAY) $I) (($DIFF SIMP) $F (($CARTAN_COORDS SIMP ARRAY) $I)))
 $I 1 $CARTAN_DIM)) MEXPR) 
(ADD2LNC (QUOTE (($EXT_DIFF) $F)) $FUNCTIONS) 
(MDEFPROP $LIE_DIFF_F ((LAMBDA) ((MLIST) $V $F) ((MPLUS SIMP) (($\| SIMP) $V 
(($EXT_DIFF SIMP) $F)) (($EXT_DIFF SIMP) (($\| SIMP) $V $F)))) MEXPR) 
(ADD2LNC (QUOTE (($LIE_DIFF_F) $V $F)) $FUNCTIONS) 
(MDEFPROP $LIE_DIFF_V ((LAMBDA) ((MLIST) $V $W) ((MPROG SIMP) ((MLIST SIMP) 
$I $J $EXT101) ((MSETQ SIMP) $EXT101 ((MLIST SIMP))) ((MDO SIMP) 
$I 1 NIL NIL $CARTAN_DIM NIL ((MSETQ SIMP) $EXT101 (($ENDCONS SIMP) 
(($SUM SIMP) ((MPLUS SIMP) ((MTIMES SIMP) (($DIFF SIMP)
 (($W SIMP ARRAY) $I) (($CARTAN_COORDS SIMP ARRAY) $J)) (($V SIMP ARRAY) $J))
 ((MTIMES SIMP) -1 (($DIFF SIMP) (($V SIMP ARRAY) $I) 
(($CARTAN_COORDS SIMP ARRAY) $J)) (($W SIMP ARRAY) $J))) $J 1 $CARTAN_DIM) $EXT101)))
 ((MRETURN SIMP) (($EXPAND SIMP) $EXT101)))) MEXPR) 
(ADD2LNC (QUOTE (($LIE_DIFF_V) $V $W)) $FUNCTIONS) 
(MDEFPROP $EDIT ((LAMBDA) ((MLIST) $F) ((MPROG SIMP) ((MLIST SIMP) $I
 $EXT101 $EXT102 $EXT103 $EXT104 $EXT105) ((MSETQ SIMP) $EXT101 0)
 ((MSETQ SIMP) $EXT102 (($EXPAND SIMP) $F)) ((MDO SIMP) $I $CARTAN_DIM -1 
NIL 1 NIL ((MPROGN SIMP) ((MSETQ SIMP) $EXT103 (($EXPAND SIMP) 
(($BOTHCOEF SIMP) $EXT102 (($CARTAN_BASIS SIMP ARRAY) $I)))) ((MSETQ SIMP)
 $EXT104 (($FIRST SIMP) $EXT103)) ((MCOND SIMP) ((MNOTEQUAL SIMP)
 $EXT104 0) ((MPROGN SIMP) ((MSETQ SIMP) $EXT102 (($LAST SIMP) $EXT103))
 ((MSETQ SIMP) $CARTAN_DIM ((MPLUS SIMP) -1 $I)) ((MSETQ SIMP) $EXT105
 (($EDIT SIMP) $EXT104)) ((MSETQ SIMP) $CARTAN_DIM $EXTDIM) ((MCOND SIMP) 
((MEQUAL SIMP) $EXT105 0) ((MSETQ SIMP) $EXT101 ((MPLUS SIMP) $EXT101
 ((MTIMES SIMP) $EXT104 (($CARTAN_BASIS SIMP ARRAY) $I)))) T ((MCOND SIMP)
 ((MEQUAL SIMP) (($INPART SIMP) $EXT105 0) "+") ((MSETQ SIMP) $EXT101
 ((MPLUS SIMP) $EXT101 (($MULTTHRU SIMP) ((MTIMES SIMP) $EXT105
 (($CARTAN_BASIS SIMP ARRAY) $I))))) T ((MSETQ SIMP) $EXT101 ((MPLUS SIMP)
 $EXT101 ((MTIMES SIMP) $EXT105 (($CARTAN_BASIS SIMP ARRAY) $I))))))) 
T $FALSE))) ((MRETURN SIMP) $EXT101))) MEXPR) 
(ADD2LNC (QUOTE (($EDIT) $F)) $FUNCTIONS) 
(MDEFPROP $BASUB ((LAMBDA) ((MLIST) $F $G $H) ((MPROG SIMP) 
((MLIST SIMP) $I $EXT101 $EXT102 $EXT103 $EXT104) ((MSETQ SIMP)
 $EXT101 (($EXPAND SIMP) $H)) ((MSETQ SIMP) $EXT102 (($EXPAND SIMP)
 (($BOTHCOEF SIMP) $EXT101 $G))) ((MSETQ SIMP) $EXT103 (($FIRST SIMP)
 $EXT102)) ((MCOND SIMP) ((MEQUAL SIMP) $EXT103 0) ((MRETURN SIMP) $H)
 T $FALSE) ((MCOND SIMP) ((MEQUAL SIMP) $G (($CARTAN_BASIS SIMP ARRAY) 1))
 ((MRETURN SIMP) ((MPLUS SIMP) (($LAST SIMP) $EXT102) (($~ SIMP) $F
 $EXT103))) T $FALSE) ((MDO SIMP) $I 2 NIL NIL $CARTAN_DIM NIL ((MCOND SIMP)
 ((MEQUAL SIMP) $G (($CARTAN_BASIS SIMP ARRAY) $I)) ((MRETURN SIMP) 
((MSETQ SIMP) $EXT104 ((MPLUS SIMP) (($LAST SIMP) $EXT102) (($~ SIMP)
 $F (($SUBSTITUTE SIMP) (($EXTSUB SIMP ARRAY) $I) $EXT103))))) 
T $FALSE)) ((MRETURN SIMP) $EXT104))) MEXPR) 
(ADD2LNC (QUOTE (($BASUB) $F $G $H)) $FUNCTIONS) 


(meval '((MDEFINE) (($LIE_DIFF) $V $X)
         ((MCOND) (($LISTP) $X) (($LIE_DIFF_V) $V $X) T
          (($LIE_DIFF_F) $V $X))))

; The following is a hand translation (more or less) of this MAXIMA code:
; 
; init_cartan(coords):=block
; (
;   [ci],
;   cartan_coords:coords,
;   cartan_dim:extdim:length(cartan_coords),
;   cartan_basis:extsubb[1]:[],
;   for i thru cartan_dim do
;   (
;     ci:concat(zzz,i),
;     cartan_basis:endcons(ci,cartan_basis),
;     extsub[i+1]:cons(ci=-ci,extsub[i]),
;     extsubb[i]:cons(ci=0,extsub[i]),
;     apply('alias,[concat(d,cartan_coords[i]),ci])
;   )
; );

(defun $init_cartan (c)
  (setq $cartan_coords c)
  (setq $cartan_dim ($length $cartan_coords))
  (setq $extdim $cartan_dim)
  (setq $cartan_basis nil)
  (meval (list '(msetq) '(($extsub array) 1) '((mlist simp))))
  (meval (list '(msetq) '(($extsubb array) 1) '((mlist simp))))

  (do
    ((c (cdr $cartan_coords) (cdr c)) (i 1 (1+ i)) (ci))
    ((null c) (setq $cartan_basis (cons '(mlist simp) (reverse $cartan_basis))))
    (setq ci ($concat 'zzz (car c)))
    (setq $cartan_basis (cons ci $cartan_basis))

    (meval (list '(msetq) (list '($extsub array) (1+ i))
           ($cons (list '(mequal simp) ci (list '(mtimes simp) -1 ci))
                  (meval (list '($extsub array) i))
           )
    ))
    (meval (list '(msetq) (list '($extsubb array) i)
           ($cons (list '(mequal simp) ci 0)
                  (meval (list '($extsub array) i))
           )
    ))
    (meval (list '(alias) ($concat 'd (car c)) ci))
  )
)
