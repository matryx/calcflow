;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 8 -*- ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defvar *macro-file* nil)

#+gcl
(progn 
  (lisp:clines "#define MAKE_UNSPECIAL(x) (check_type_symbol(&(x)),(x)->s.s_stype = stp_ordinary, Cnil)")
  (lisp:defentry make-unspecial (lisp:object) (lisp:object "MAKE_UNSPECIAL")))

#+(or scl cmu)
(defun make-unspecial (symbol)
  (ext:clear-info variable c::kind symbol)
  symbol)


(defmacro declare-top (&rest decl-specs)
  `(eval-when
    ,(cond (*macro-file*  #+gcl '(compile eval load)
			  #-gcl '(:compile-toplevel :load-toplevel :execute) )
	   (t #+gcl '(eval compile) #-gcl '(:compile-toplevel :execute)))
    ,@(loop for v in decl-specs
	     unless (member (car v) '(special unspecial)) nconc nil
	     else
	     when (eql (car v) 'unspecial)
	     collect `(progn
		       ,@(loop for w in (cdr v)
				collect #-(or gcl scl cmu ecl)
                                       `(remprop ',w
						 #-excl 'special
						 #+excl 'excl::.globally-special.)
				#+(or gcl scl cmu ecl)
			        `(make-unspecial ',w)))
	     else collect `(proclaim ',v))))

;;; This list should contain all specials required by runtime or more than one maxima file,
;;; except for some specials declared in the macro files, eg displm

(declaim (special
	  $% $%% $%edispflag $%emode $%enumer $%e_to_numlog $%iargs $%piargs
	  $%rnum $%rnum_list $absboxchar $activecontexts $algebraic $algexact
	  $aliases $arrays $askexp $assumescalar $backsubst $berlefact
	  $beta_args_sum_to_integer $bftorat $bftrunc $boxchar
	  $breakup $cauchysum $change_filedefaults $compgrind $context
	  $contexts $current_let_rule_package $debugmode
	  $default_let_rule_package $demoivre $dependencies $derivabbrev
	  $derivsubst $detout $dispflag $display2d $displayset
	  $display_format_internal $doallmxops $domain $domxexpt $domxmxops
	  $domxnctimes $domxplus $domxtimes $dontfactor $doscmxops
	  $doscmxplus $dot0nscsimp $dot0simp $dot1simp $dotassoc
	  $dotconstrules $dotdistrib $dotexptsimp $dotident $dotscrules
	  $erfflag $errexp $error $error_size $error_syms $expon
	  $exponentialize $expop $exptdispflag $exptisolate $exptsubst
	  $facexpand $factorflag $features $file_search
	  $float $float2bf $floatformat $floatfrac $floatint
	  $floatoptions $floatprec $floatwidth $fortfloat $fortindent
	  $fortspaces $fpprec $fpprintprec $fptrunc $functions $gammalim $gcd
	  $genindex $gensumnum $globalsolve $gradefs $halfangles $homog_hack
	  $inchar $infeval $inflag $intfaclim
	  $isolate_wrt_times $keepfloat $labels $leftjust $letrat $letvarsimp
	  $let_rule_packages $liflag $linechar $linenum $linel
	  $linsolvewarn $linsolve_params $lispdisp $listarith $listconstvars
	  $lmxchar $logarc $logconcoeffp $logexpand $lognegint $lognumer
	  $logsimp $m1pbranch $macroexpansion $macros $maperror $mapprint
	  $matrix_element_add $matrix_element_mult $matrix_element_transpose
	  $maxapplydepth $maxapplyheight $maxnegex $maxposex $maxprime
	  $maxtayorder $mode_checkp $mode_check_errorp $mode_check_warnp
	  $morewait $multiplicities $mx0simp $myoptions $nalgfac $negdistrib
	  $negsumdispflag $newfac $nolabels $norepeat $noundisp $numer
	  $numer_pbranch $operators $opsubst $optimprefix $optionset $outchar
	  $pagepause $parsewindow $partswitch $pfeformat $piece $pointbound
	  $poislim $powerdisp $prederror $programmode $props
	  $radexpand $ratalgdenom $ratdenomdivide $ratepsilon $ratexpand
          $ratfac $ratmx $ratprint $ratsimpexpons $ratvars $ratvarswitch
          $ratweights
	  $ratwtlvl $realonly $refcheck $resultant $rmxchar $rootsconmode
	  $rules $savedef $savefactors $scalarmatrixp $setcheck
	  $setcheckbreak $setval $showtime $signbfloat $simp $simpsum
	  $solvedecomposes $solveexplicit $solvefactors $solvenullwarn
	  $solveradcan $solvetrigwarn $sparse
	  $sqrtdispflag $stardisp $sublis_apply_lambda
	  $subnumsimp $sumexpand $sumsplitfact
	  $superlogcon $suspend $taylor_logexpand
	  $taylor_truncate_polynomials $timer $timer_devalue
	  $to_call_lisp_compiler $trace $trace_break_arg $trace_max_indent
	  $trace_safety $transcompile $translate $transrun
	  $trigexpand $trigexpandplus $trigexpandtimes $triginverses
	  $trigsign $tr_array_as_ref $tr_bound_function_applyp
	  $tr_file_tty_messagesp $tr_float_can_branch_complex
	  $tr_function_call_default $tr_gen_tags $tr_numer
	  $tr_optimize_max_loop $tr_output_file_default
	  $tr_predicate_brain_damage $tr_semicompile $tr_state_vars
	  $tr_true_name_of_file_being_translated $tr_warn_bad_function_calls
	  $tr_warn_fexpr $tr_warn_meval $tr_warn_mode $tr_warn_undeclared
	  $tr_warn_undefined_variable $ttyintfun $ttyintnum
	  $universe $user_mesfile $use_fast_arrays $values $vect_cross
	  $zerobern %e-val %p%i %pi-val %pi//2 %pi//4 %pi2 *$any-modes*
	  *alpha *const* *fnewvarsw *gcdl* *in *in-$batchload* *in-compile*
	  *in-macsyma-indexer* *in-translate-file* *inv* *irreds *min* *mx*
	  *n *opers-list *out *ratweights *tr-warn-break* *transl-backtrace*
	  *transl-debug* *transl-file-debug* *warned-fexprs*
	  *warned-mode-vars* *warned-un-declared-vars* *zexptsimp? |-1//2|
	  -sqrt3//2 |1//2| adn* aexprp algfac* algnotexact
	  alpha *alphabet* arrays aryp assigns *atp* atvars *baktrcl*
	  bfhalf bfmhalf bigfloat%e bigfloat%pi bigfloatone bigfloatzero
	  bindlist bvars *mdebug*
	  declares defined_variables defintdebug derivflag derivlist
	  derivsimp displayp dn* dosimp dsksetp dummy-variable-operators
	  dumping envlist errorsw errrjfflag evarrp evp expandflag expandp
	  exprs exptrlsw factlist featurel fexprs fmaplvl
	  forms-to-compile-queue fourth%pi fr-factor gauss
	  generate-atan2 genpairs genvar half%pi half%pi3 hmodulus
	  implicit-real in-p infinitesimals infinities inratsimp inside-mprog
	  integerl *islinp* lexprs limit-answers limitp linel
	  *linelabel* local loclist low* maplp master-mesfile mdop
	  meta-prop-l meta-prop-p mfexprp minpoly* mlocp mm* modulus *mopl*
	  mplc* mprogp mproplist mspeclist mspeclist2 msump munbindp
	  need-prog? negprods negsums nn* noevalargs noitems nonintegerl
	  *nounl* *nounsflag* opers opers-list outargs1 outargs2
	  preserve-direction prods putl radcanp rd*
	  real-infinities realonlyratnum *refchkl* return-mode returns rulefcnl
	  rulesw scanmapp sfindex sign-imag-errp simplimplus-problems
	  *small-primes* specials sqrt3//2 state-pdl $stringdisp substp
	  sums tellratlist timesinp tr-abort tr-progret tr-unique
	  transl-file translate-time-evalables transp
	  tstack typel user-mesfile user-timesofar varlist wflag
	  $wtlevel $cflength $weightlevels *trunclist $taylordepth
	  $maxtaydiff $verbose $psexpand ps-bmt-disrep
	  silent-taylor-flag $define_variable $infolists))

(declaim (declaration unspecial))
