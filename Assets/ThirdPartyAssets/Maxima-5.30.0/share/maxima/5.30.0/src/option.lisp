;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OPTIONS functions
;;; (c) Copyright 1980, Massachusetts Institute of Technology

(in-package :maxima)

(macsyma-module option)

(declare-top (special options history cru crf))

(defmspec $options (x)
  (setq x (cdr x))
  (cond ((null x)
	 (princ "`options' interpreter  (Type `exit' to exit.)")
	 (terpri) (options '$all))
	((nonsymchk (car x) 'options))
	(t (cons '(mlist) (downs (car x))))))

(defun options (ans)
  (do ((history)) (nil)
    (cond ((eq '$exit ans) (return '$done))
	  ((or (eq '$up ans) (eq '$back ans))
	   (setq ans (cadr history) history (cddr history))
	   (down (if (null ans) '$all ans)))
	  ((eq '$top ans) (options '$all))
	  ((atom ans) (down ans))
	  ((or (eq '$down (caar ans)) (eq '$options (caar ans)))
	   (down (cadr ans)))
	  ((eq '$describe (caar ans)) (mdescribe (decode (cadr ans))))
	  (t (opt-err)))
    (setq ans (retrieve ": " nil))))

(defun down (node &aux opts)
  (setq node (decode node) opts (downs node))
  (cond ((null opts) (princ "No options") (terpri))
	(t (setq history (cons node history) options opts)
	   (menu options))))

(defun up (node &aux opts)
  (setq node (decode node) opts (ups node))
  (cond ((null opts) (princ "No options") (terpri))
	(t (setq history (cons node history) options opts)
	   (menu options))))

(defun downs (node) (oldget node 'subc))

(defun ups (node) (oldget node 'supc))

(defun decode (node)
  (cond ((not (integerp node)) node)
	((or (zerop node) (null (setq node (nthcdr (1- node) options)))) (nor-err))
	(t (car node))))

(defun menu (opts)
  (do ((l opts (cdr l)) (i 1 (f1+ i))) ((null l))
    (princ i) (princ " - ") (princ (fullstrip1 (car l)))
    (cond ((oldget (car l) 'kind) (write-char #\space) (princ (oldget (car l) 'kind))))
    (terpri)))


(defun opt-err () (princ "Illegal command to `options'") (terpri))

(defun nor-err () (princ "Number out of range") (terpri))

(defmacro subc (a b &rest l)
  `(subc-internal '(,a ,b ,@l)))

(defun subc-internal (x)
  (putprop (car x) (cadr x) 'kind)
  (putprop (car x) (cddr x) 'subc))

(defmacro supc (a b &rest l)
  `(supc-internal '(,a ,b ,@l)))

(defun supc-internal (x)
  (putprop (car x) (cadr x) 'kind)
  (putprop (car x) (cddr x) 'supc))

(defun printnet () (prnet '$all 0) nil)

(defun prnet (node indent)
  (terpri)
  (do ((i 1 (1+ i)))
      ((> i indent))
    (write-char #\tab))
  (princ (fullstrip1 node))
  (cond ((oldget node 'kind) (write-char #\space) (princ (oldget node 'kind))))
  (mapc #'(lambda (l) (prnet l (1+ indent))) (downs node)))

;;Copyright 1980, Massachusetts Institute of Technology
(subc $all () $interaction $debugging $evaluation $lists $matrices 
      $simplification $representations $plotting $translation
      $pattern-matching $tensors)

(subc $abs (c))

(subc $addrow (c))

(subc $alarmclock (c))

(subc $aliases (v))

(subc $algsys (c))

(subc $allroots (c))

(subc $append (c))

(subc $appendfile (c))

(subc $apply (c))

(supc $apropos (c) $user-aids $general-info)

(subc $arrays () $array)

(subc $arrays (v))

(subc $array (c))

(subc $arrayinfo (c))

(subc $arraymake (c))

(subc $assume (c))

(subc $at (c))

(subc $atom (c))

(subc $atvalue (c))

(subc $augcoefmatrix (c))

(subc $automatic () $trigexpand $triginverses $trigsign $exponentialize
      $logarc $demoivre $logexpand $radexpand)

(subc $baksolve (c))

(subc $batch (c))

(subc $batcon (c))

(subc $bern (c))

(subc $bernpoly (c))

(subc $beta (c))

(subc $bfloat (c))

(supc $bftrunc (s) $display)

(subc $break (c))

(subc $cabs (c))

(subc $catch (c))

(subc $cf (c))

(subc $cfdisrep (c))

(subc $cfexpand (c))

(subc $changevar (c))

(subc $charpoly (c))

(subc $chr1 (c))

(subc $chr2 (c))

(subc $christof (c))

(subc $closefile (c))

(subc $coeff (c))

(subc $coefmatrix (c))

(subc $col (c))

(subc $comexp (c))

(subc $compile (c))

(subc $concat (c))

(subc $command-files (c) $batch $batcon $demo)

(subc $compfile (c))

(subc $complex () $realpart $imagpart $rectform $polarform $cabs)

(subc $cons (c))

(subc $constantp (c))

(subc $content (c))

(subc $contract (c))

(subc $copylist (c))

(subc $covdiff (c))

(subc $copymatrix (c) $ratmx $sparse $listarith $detout $doallmxops
      $domxmxops $doscmxplus $scalarmatrixp)

(subc $curvature () $scurvature $riemann $raiseriemann $rinvariant $weyl
      $dscalar $dalem $yt)

(subc $dalem (c))

(subc $debugmode (c))

(subc $declare (c))

(subc $defcon (c))

(subc $define (c))

(subc $defmatch (c))

(subc $defrule (c))

(subc $deftaylor (c))

(subc $debugging () $trace $debug $debugmode $break $bindtest $optionset)

(subc $delete (c))

(supc $derivabbrev (s) $display)

(subc $defile (c))

(subc $delta (c))

(subc $demo (c))

(subc $denom (c))

(subc $depends (c))

(subc $dependencies (v))

(subc $derivdegree (c))

(subc $determinant (c))

(subc $describe (c))

(subc $diagmatrix (c))

(subc $diff (c) $dependencies $gradef)

(subc $display (c) $powerdisp $sqrtdispflag $stardisp $derivabbrev 
      $exptdispflag $%edispflag $bftrunc $pfeformat
      $noundisp $nolabels)

(subc $disp (c))

(subc $dispfun (c))

(subc $disprule (c))

(subc $dispterms (c))

(subc $divide (c))

(subc $dpart (c))

(subc $dscalar (c))

(subc $dummy (c))

(subc $editing () $macsyma-line-editor $teco)

(supc $%edispflag (s) $display)

(subc $echelon (c))

(subc $einstein (c) $rateinstein $facrat)

(subc $ematrix (c))

(subc $endcons (c))

(subc $entermatrix (c) $ratmx $sparse $listarith $detout $doallmxops
      $domxmxops $doscmxplus $scalarmatrixp)

(subc $entier (c))

(subc $equal (c))

(subc $erf (c))

(subc $errcatch (c))

(subc $error (c))

(subc $euler (c))

(subc $exp (c))

(subc $explicit () $tsetup $quantities $curvature)

(subc $extend (c))

(subc $ezgcd (c))

(subc $expand (c) $maxposex $maxnegex)

(subc $expansion () $expand $ratexpand)

(supc $exponentialize (s) $ev $simplification)

(supc $exptdispflag (s) $display)

(subc $ev (c) $exponentialize $%iargs $logarc $%piargs $trigsign 
      $triginverses)

(subc $evaluation () $variable $function $array $simp)

(supc $facrat (s) $einstein $reimann $weyl)

(subc $factcomb (c))

(subc $factor (c) $ratvars)

(subc $factorsum (c) $ratvars)

(subc $factoring () $factor $gfactor $factorsum $gfactorsum $sqfr $partition)

(subc $fassave (c))

(subc $fasttimes (c))

(subc $fib (c))

(subc $first (c))

(subc $float (c))

(subc $floatnump (c))

(subc $forget (c))

(subc $fpprec (c))

(subc $freeof (c))

(subc $files () $file-creation $file-deletion $save-files $command-files)

(subc $file-creation () $appendfile $closefile $fassave $store
      $save $writefile)

(subc $file-deletion () $defile $remfile)

(subc $fullmap (c) $maperror $maprat)

(subc $fullmapl (c) $maperror $maprat)

(subc $functions (v))

(subc $gamma (c))

(subc $gcd (c))

(subc $general-info () $describe $example $options $primer $apropos)

(subc $genfact (c))

(subc $genmatrix (c) $ratmx $sparse $listarith $detout $doallmxops
      $domxmxops $doscmxplus $scalarmatrixp)

(subc $get (c))

(subc $getchar (c))

(subc $gfactor (c) $ratvars)

(subc $gfactorsum (c) $ratvars)

(subc $gradef (c))

(subc $gradefs (v))

(subc $graph (c) $plotheight $linel)

(subc $hipow (c))

(subc $horner (c))

(supc $%iargs (s) $ev $simplification)

(supc $inchar (v))

(subc $ident (c))

(subc $ilt (c))

(subc $imagpart (c))

(subc $indices (c))

(subc $inpart (c))

(subc $integerp (c))

(subc $integrate (c))

(subc $intopois (c))

(subc $infix (c))

(subc $input () $syntax $editing $retrieve $read $inchar)

(subc $interaction () $input $output $files $information $user-aids
      $information)

(subc $infolists (v))

(subc $information () $general-info $specific-info $information-lists)

(subc $information-lists () $infolists $myoptions $aliases $labels
      $labels $values $functions $rules $props
      $matchdeclares $modedeclares $arrays
      $gradefs $dependencies)
(subc $is (c) $prederror)

(subc $isolate (c))

(subc $isqrt (c))

(subc $kill (c))

(subc $labels (v))

(subc $lambda (c))

(subc $laplace (c))

(subc $last (c))

(subc $lc (c))

(subc $ldefint (c))

(subc $ldisp (c))

(subc $ldisplay (c))

(subc $length (c))

(subc $let (c))

(subc $letrules (c))

(subc $letsimp (c))

(subc $lhs (c))

(subc $limit (c))

(supc $linel (v) $display $plot $graph $multigraph $paramplot)

(subc $lists () $cons $endcons $append $member $reverse $first $rest
      $last $delete $length $mapping)

(supc $listarith (s) $entermatrix $matrix $genmatrix $copymatrix
      $addrow $transpose $echelon $triangularize
      $rank $determinant $charpoly)

(subc $linsolve (c))

(subc $listofvars (c))

(subc $listp (c))

(subc $loadfile (c))

(subc $local (c))

(subc $log (c))

(subc $logout (c))

(subc $lopow (c))

(subc $lorentz (c))

(subc $lpart (c))

(subc $lriccicom (c))

(supc $logarc (s) $ev $simplification)

(subc $macsyma-line-editor ())

(subc $makebox (c))

(subc $makenonscalar (c))

(subc $map (c) $maperror $maprat)

(subc $maplist (c) $maperror $maprat)

(supc $maperror (s) $map $maplist $fullmap $fullmapl)

(subc $mapping () $map $maplist $fullmap $fullmapl $scanmap)

(supc $maprat (s) $map $maplist $fullmap $fullmapl)

(subc $matchdeclares (v))

(subc $matchdeclare (c))

(subc $matchfix (c))

(subc $matrices () $matrix-construction $matrix-manipulation)

(subc $matrix-construction () $entermatrix $matrix $genmatrix $copymatrix
      $addrow)

(subc $matrix-manipulation () $transpose $echelon $triangularize
      $rank $determinant $charpoly)

(subc $matrix (c) $ratmx $sparse $listarith $detout $doallmxops
      $domxmxops $doscmxplus $scalarmatrixp)

(subc $matrixmap (c))

(subc $matrixp (c))

(subc $max (c))

(subc $maxnegex (s) $expand)

(subc $maxposex (s) $expand)

(subc $member (c))

(subc $min (c))

(subc $minfactorial (c))

(subc $minor (c))

(subc $polymod (c))

(subc $modedeclare (c))

(subc $modedeclares (v))

(subc $motion (c))

(subc $multigraph (c) $plotheight $linel)

(subc $multthru (c))

(subc $myoptions (v))

(subc $nary (c))

(subc $newdet (c))

(subc $nonscalarp (c))

(subc $nounify (c))

(subc $nofix (c))

(supc $nolabels (s) $display)

(supc $noundisp (s) $display)

(subc $nroots (c))

(subc $nterms (c))

(subc $ntermsg (c))

(subc $ntermsrci (c))

(subc $num (c))

(subc $numberp (c))

(subc $numerval (c))

(subc $numfactor (c))

(subc $optimize (c))

(subc $options (c) $down $up $back $describe $exit)

(subc $ordergreat (c))

(subc $orderless (c))

(subc $other-transformations () $trigreduce $trigexpand $factcomb $logcontract)

(subc $outofpois (c))

(subc $outchar (v))

(subc $output () $print $display $outchar)

(subc $paramplot (c) $plotheight $linel)

(subc $part-functions () $part $inpart $lhs $rhs %num $denom $coeff $$first
      $rest $last $ratcoef)

(subc $part (c))

(subc $partfrac (c))

(subc $partition (c)  $ratvars)

(supc $pfeformat (s) $display)

(supc $%piargs (s) $ev $simplification)

(subc $pickapart (c))

(subc $playback (c))

(subc $plog (c))

(subc $plot (c) $plotheight $linel)

(supc $plotheight (v) $plot $graph $multigraph $paramplot)

(subc $plotting () $plot $graph $multigraph $paramplot)

(subc $poisdiff (c))

(subc $poisexpt (c))

(subc $poisint (c))

(subc $poismap (c))

(subc $poisplus (c))

(subc $poissimp (c))

(subc $poissubst (c))

(subc $poistimes (c))

(subc $poistrim (c))

(subc $polarform (c))

(subc $polysign (c))

(subc $postfix (c))

(supc $powerdisp (s) $display)

(subc $powerseries (c))

(supc $prederror (s) $is)

(subc $predicates () $is $zeroequiv $assume $forget)

(subc $prefix (c))

(subc $primer (c))

(subc $print (c))

(subc $printpois (c))

(subc $printprops (c))

(subc $product (c))

(subc $props (v))

(subc $properties (c))

(subc $propvars (c))

(subc $psi (c))

(subc $put (c))

(subc $qput (c))

(subc $quantities () $christof $motion $riccicom $ntermsrci $lriccicom
      $einstein $ntermsg)

(subc $quit (c))

(subc $qunit (c))

(subc $quotient (c))

(subc $radcan (c))

(subc $raiseriemann (c))

(subc $random (c))

(subc $rank (c))

(subc $rat (c))

(subc $ratcoef (c))

(subc $ratdenom (c))

(subc $ratdiff (c))

(subc $ratdisrep (c))

(supc $rateinstein (s) $einstein)

(subc $ratexpand (c))

(subc $rational () $expand $multthru $xthru $combine $factor $factorsum
      $factorout $sqfr $ratsimp $partfrac)

(supc $ratmx (s) $entermatrix $matrix $genmatrix $copymatrix
      $addrow $transpose $echelon $triangularize
      $rank $determinant $charpoly)

(subc $ratnumer (c))

(subc $ratnump (c))

(subc $ratp (c))

(supc $ratriemann (s) $riemann)

(subc $ratsimp (c))

(subc $ratsubst (c))

(supc $ratvars (v))

(subc $ratweight (c))

(supc $ratweyl (s) $weyl)

(subc $read (c))

(subc $realpart (c))

(subc $realroots (c))

(subc $rectform (c))

(subc $rem (c))

(subc $remainder (c))

(subc $remarray (c))

(subc $rembox (c))

(subc $remcon (c))

(subc $remfile (c))

(subc $remfunction (c))

(subc $remlet (c))

(subc $remove (c))

(subc $remrule (c))

(subc $retrieve (c))

(subc $remtrace (c))

(subc $remvalue (c))

(subc $rename (c))

(subc $reset (c))

(subc $residue (c))

(subc $representations () $general $cre $transformations $substitutions
      $part-functions)

(subc $rest (c))

(subc $reverse (c))

(subc $restore (c))

(subc $resultant (c))

(subc $reveal (c))

(subc $rhs (c))

(subc $riccicom (c))

(subc $riemann (c) $ratriemann $facrat)

(subc $rinvarient (c))

(subc $risch (c))

(subc $row (c))

(subc $rules (v))

(subc $save-files (c) $loadfile $restore)

(subc $save (c))

(subc $scanmap (c))

(subc $scurvature (c))

(subc $send (c))

(subc $setelmx (c))

(subc $setup (c))

(subc $show (c))

(subc $showtime (c))

(subc $sign (c))

(subc $signum (c))

(subc $simp (s))

(subc $simplification () $automatic $simp-rules)

(subc $solve (c))

(supc $sparse (s) $entermatrix $matrix $genmatrix $copymatrix
      $addrow $transpose $echelon $triangularize
      $rank $determinant $charpoly)

(subc $specific-info () $trace $untrace $grind $disprule $properties
      $printprops $playback $dispfun $arrayinfo)

(subc $sqfr (c) $ratvars)

(subc $sqrt (c))

(subc $srrat (c))

(subc $stardisp (c))

(subc $status (c))

(subc $store (c))

(subc $string (c))

(subc $stringout (c))

(subc $submatrix (c))

(subc $subst (c))

(subc $substinpart (c))

(subc $substitutions () $subst $ratsubst $substpart $substinpart)

(subc $substpart (c))

(subc $sum (c))

(subc $symbol (c))

(subc $syntax () $prefix $infix $postfix $nary $matchfix $nofix $symbol)

(subc $taylor (c))

(subc $tellrat (c))

(subc $tellsimp (c))

(subc $tellsimpafter (c))

(subc $tensors () $explicit $indicial)

(subc $throw (c))

(subc $tldefint (c))

(subc $tlimit (c))

(subc $totaldisrep (c))

(subc $trace (c) $untrace $remtrace)

(subc $transformations () $rational $other-transformations)

(subc $translate (c) $transrun $modedeclare)

(subc $transpose (c))

(supc $transrun (s) $evaluation)

(subc $triangularize (c))

(subc $trig () $trigswitches $trigexpand $trigreduce)

(subc $trigexpand (c s))

(supc $triginverses (s) $ev $simplification)

(subc $trigreduce (c))

(supc $trigsign (s) $ev $simplification)

(subc $trigswitches () $%piargs $%iargs $triginverses $trigsign 
      $exponentialize $logarc)

(subc $tsetup (c))

(subc $scanmap (c))

(subc $simplification () $expansion $factoring $trig)

(subc $solve (c) $solvefactors $solveradcan)

(supc $solvefactors (s) $solve)

(supc $solveradcan (s) $solve)

(supc $sqrtdispflag (s) $display)

(supc $stardisp (s) $display)

(subc $translation () $translate $compfile $modedeclare)

(subc $trigfunction () %sin %cos %tan %cot %csc %sec 
      %asin %acos %atan %acot %acsc %asec
      %sinh %cosh %tanh %coth %csch %sech
      %asinh %acosh %atanh %acoth %acsch %asech)

(subc $undiff (c))

(subc $universals () $timedate $who $bug $mail $send)

(subc $unorder (c))

(subc $unstore (c))

(subc $untrace (c))

(subc $user-aids () $primer $describe $options $example $apropos $visual-aids)

(subc $values (v))

(subc $verbify (c))

(subc $visual-aids () $reveal $isolate $pickapart)

(subc $weyl (c) $ratweyl $facrat)

(subc $writefile (c))

(subc $xthru (c))

(subc $yt (c))

(subc $zeta (c))

(subc $zeroequiv (c))

(subc %sin (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign
      $logarc)

(subc %cos (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign
      $logarc)

(subc %tan (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign
      $logarc)

(subc %cot (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign
      $logarc)

(subc %csc (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign
      $logarc)

(subc %sec (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign
      $logarc)

(subc %asin (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %acos (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %atan (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %acot (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %acsc (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %asec (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %sinh (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign)

(subc %cosh (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign)

(subc %tanh (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign)

(subc %coth (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign)

(subc %csch (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign)

(subc %sech (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigexpand $exponentialize $halfangles $trigsign)

(subc %asinh (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %acosh (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %atanh (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %acoth (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %acsch (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc %asech (c) $float $numer $bfloat $%piargs $%iargs $triginverses
      $trigsign $logarc)

(subc |.| (c) $dotassoc $dotscrules $dotconstrules $dotexptsimp
      $dotdistrib $assumescalar)

