;;; maxima-font-lock.el --- syntax highlighting for maxima.el

;; Copyright: (C) 2001 Jay Belanger

;; Author: Jay Belanger <belanger@truman.edu>
;; $Name:  $
;; $Revision: 1.21 $
;; $Date: 2010-11-21 21:42:45 $
;; Keywords: maxima, font-lock

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <belanger@truman.edu>
;; The latest version of this package should be available at
;; ftp://vh213601.truman.edu/pub/Maxima

;;; Commentary

;;; This file is used for font-lock for maxima.el
;;
;; The keywords are divided into the following groups, following the 
;; Maxima info files:
;; Functions (font-lock-builtin-face or font-lock-keyword-face)
;; Variables (font-lock-keyword-face)
;; Constants (font-lock-constant-face)
;; Keywords (font-lock-keyword-face)
;; Declarations (font-lock-keyword-face)
;; Operators (font-lock-keyword-face)
;; Property (font-lock-keyword-face)
;; Macros (font-lock-keyword-face)
;; Special operators (font-lock-keyword-face)
;; Special symbols (font-lock-keyword-face)
;;

;;; Code

(require 'font-lock)
(provide 'maxima-font-lock)


;;; The faces
;; (defvar maxima-variable-face 'maxima-variable-face
;;   "The face to use for the variables.")

;; (defvar maxima-function-face 'maxima-function-face
;;   "The face to use for the functions.")

;; (defvar maxima-constant-face 'maxima-constant-face
;;   "The face to use for the constants.")

;; (defvar maxima-keyword-face 'maxima-keyword-face
;;   "The face to use for the keywords.")

;; (defvar maxima-operator-face 'maxima-operator-face
;;   "The face to use for the operators.")

;; (defvar maxima-property-face 'maxima-property-face
;;   "The face to use for the properties.")

;; (defvar maxima-macro-face 'maxima-macro-face
;;   "The face to use for the macros.")

;; (defvar maxima-specop-face 'maxima-specop-face
;;   "The face to use for the special operators.")

;; (defvar maxima-declaration-face 'maxima-declaration-face
;;   "The face to use for the macros.")

;; (defvar maxima-specsymb-face 'maxima-specsymb-face
;;   "The face to use for the special symbols.")

;;; the regexps
(defvar maxima-vars-1
  (list
   "%"
   "%%"
   "%edispflag"
   "%rnum_list"))


(defvar maxima-match-variables-1
  (concat "\\<\\(" 
          (eval-when-compile (regexp-opt maxima-vars-1))
          "\\)\\>")
  "regexp to match the maxima variables.")

(defvar maxima-vars-2
  (list
   "all_dotsimp_denoms"
   "assume_pos"
   "assume_pos_pred"
   "change_filedefaults"
   "current_let_rule_package"
   "default_let_rule_package"
   "display_format_internal"
   "error_size"
   "error_syms"
   "expandwrt_denom"
   "file_search"
   "file_string_print"
   "in_netmath"
   "integration_constant_counter"
   "isolate_wrt_times"
   "let_rule_packages"
   "linsolve_params"
   "matrix_element_add"
   "matrix_element_mult"
   "matrix_element_transpose"
   "mode_checkp"
   "mode_check_errorp"
   "mode_check_warnp"
   "plot_options"
   "solve_inconsistent_error"
   "sublis_apply_lambda"
   "taylor_logexpand"
   "taylor_order_coefficients"
   "taylor_truncate_polynomials"
   "timer_devalue"
   "tr_array_as_ref"
   "tr_bound_function_applyp"
   "tr_file_tty_messagesp"
   "tr_float_can_branch_complex"
   "tr_function_call_default"
   "tr_gen_tags"
   "tr_numer"
   "tr_optimize_max_loop"
   "tr_output_file_default"
   "tr_predicate_brain_damage"
   "tr_semicompile"
   "tr_state_vars"
   "tr_true_name_of_file_being_translated"
   "tr_version"
   "tr_warn_bad_function_calls"
   "tr_warn_fexpr"
   "tr_warn_meval"
   "tr_warn_mode"
   "tr_warn_undeclared"
   "tr_warn_undefined_variable"
   "use_fast_arrays"))
   
   
(defvar maxima-match-variables-2
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-vars-2))
          "\\)\\>")
  "regexp to match the maxima variables.")

(defvar maxima-vars-3
  (list
   "absboxchar"
   "activecontexts"
   "algebraic"
   "algepsilon"
   "algexact"
   "aliases"
   "all"
   "allsym"
   "arrays"
   "askexp"
   "assumescalar"
   "backsubst"
   "backtrace"
   "batchkill"
   "batcount"
   "berlefact"
   "bftorat"
   "bftrunc"
   "bothcases"
   "boxchar"
   "breakup"
   "cauchysum"
   "cflength"
   "compgrind"
   "context"
   "contexts"
   "counter"
   "cursordisp"
   "debugmode"
   "demoivre"
   "dependencies"
   "derivabbrev"
   "derivsubst"
   "detout"
   "diagmetric"
   "dim"
   "direc"
   "dispflag"
   "display2d"
   "doallmxops"
   "domain"
   "domxexpt"
   "domxmxops"
   "domxnctimes"
   "dontfactor"
   "doscmxops"
   "doscmxplus"
   "dot0nscsimp"
   "dot0simp"
   "dot1simp"
   "dotassoc"
   "dotconstrules"
   "dotdistrib"
   "dotexptsimp"
   "dotident"
   "dotscrules"
   "dskall"
   "erfflag"
   "errexp"
   "errintsce"
   "errorfun"
   "evflag"
   "evfun"
   "exptdispflag"
   "expon"
   "exponentialize"
   "expop"
   "exptisolate"
   "exptsubst"
   "facexpand"
   "factlim"
   "factorflag"
   "filename"
   "filenum"
   "float2bf"
   "fortindent"
   "fortspaces"
   "fpprec"
   "fpprintprec"
   "functions"
   "gammalim"
   "genindex"
   "gensumnum"
   "globalsolve"
   "gradefs"
   "halfangles"
   "ibase"
   "icounter"
   "idummyx"
   "ieqnprint"
   "inchar"
   "inflag"
   "intfaclim"
   "infolists"
   "intpolabs"
   "intpolerror"
   "intpolrel"
   "keepfloat"
   "lasttime"
   "letrat"
   "lhospitallim"
   "linechar"
   "linedisp"
   "linel"
   "linenum"
   "linsolvewarn"
   "listarith"
   "listconstvars"
   "listdummyvars"
   "lmxchar"
   "loadprint"
   "logabs"
   "logarc"
   "logconcoeffp"
   "logexpand"
   "lognegint"
   "lognumer"
   "logsimp"))   

(defvar maxima-match-variables-3
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-vars-3))
          "\\)\\>")
  "regexp to match the maxima variables.")

(defvar maxima-vars-4
  (list
   "m1pbranch"
   "macroexpansion"
   "maperror"
   "maxapplydepth"
   "maxapplyheight"
   "maxnegex"
   "maxposex"
   "maxprime"
   "maxtayorder"
   "modulus"
   "multiplicities"
   "myoptions"
   "negdistrib"
   "negsumdispflag"
   "newfac"
   "niceindicespref"
   "nolabels"
   "noundisp"
   "obase"
   "omega"
   "opproperties"
   "opsubst"
   "optimprefix"
   "optionset"
   "packagefile"
   "parsewindow"
   "partswitch"
   "pfeformat"
   "piece"
   "poislim"
   "powerdisp"
   "prederror"
   "prodhack"
   "programmode"
   "prompt"
   "psexpand"
   "radexpand"
   "radprodexpand"
   "ratalgdenom"
   "ratdenomdivide"
   "ratepsilon"
   "rateinstein"
   "ratfac"
   "ratmx"
   "ratprint"
   "ratrieman"
   "ratriemann"
   "ratsimpexpons"
   "ratweights"
   "ratweyl"
   "ratwtlvl"
   "realonly"
   "refcheck"
   "rmxchar"
   "rombergabs"
   "rombergit"
   "rombergmin"
   "rombergtol"
   "rootsconmode"
   "rootsepsilon"
   "savedef"
   "savefactors"
   "scalarmatrixp"
   "setcheck"
   "setcheckbreak"
   "setval"
   "showtime"
   "simpsum"
   "solvedecomposes"
   "solveexplicit"
   "solvefactors"
   "solvenullwarn"
   "solveradcan"
   "solvetrigwarn"
   "sparse"
   "sqrtdispflag"
   "stardisp"
   "sumexpand"
   "sumhack"
   "sumsplitfact"
   "taylordepth"
   "tlimswitch"
   "transcompile"
   "transrun"
   "trigexpandplus"
   "trigexpandtimes"
   "triginverses"
   "trigsign"
   "ttyintfun"
   "ttyintnum"
   "ttyoff"
   "undeclaredwarn"
   "values"
   "vect_cross"
   "verbose"
   "zerobern"
   "zeta%pi"
   "zunderflow"))
   

(defvar maxima-match-variables-4
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-vars-4))
          "\\)\\>")
  "regexp to match the maxima variables.")

(defvar maxima-fns-1
  (list
   "%"
   "%th"
   "%j"
   "%k"
   "?round"
   "?truncate"))
                        

(defvar maxima-match-functions-1
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-fns-1))
          "\\)\\>" )
  "regexp to match the maxima functions.")

(defvar maxima-fns-2
  (list
   "acos"
   "acosh"
   "acot"
   "acoth"
   "acsc"
   "acsch"
   "activate"
   "addcol"
   "addrow"
   "adjoint"
   "airy"
   "alarmclock"
   "algsys"
   "alias"
   "alloc"
   "allroots"
   "anti"
   "antid"
   "antidiff"
   "append"
   "appendfile"
   "apply"
   "apply1"
   "apply2"
   "applyb1"
   "apply_nouns"
   "apropos"
   "args"
   "array"
   "arrayapply"
   "arrayinfo"
   "arraymake"
   "asec"
   "asech"
   "asin"
   "asinh"
   "askinteger"
   "asksign"
   "assume"
   "asymp"
   "asympa"
   "at"
   "atan"
   "atan2"
   "atanh"
   "atom"
   "atrig1"
   "atvalue"
   "augcoefmatrix"
   "backup"
   "bashindices"
   "batch"
   "batchload"
   "batcon"
   "bern"
   "bernpoly"
   "bessel"
   "beta"
   "bezout"
   "bffac"
   "bfloat"
   "bfloatp"
   "bfpsi"
   "bfzeta"
   "bgzeta"
   "bhzeta"
   "bindtest"
   "binomial"
   "block"
   "bothcoef"
   "box"
   "break"
   "bug"
   "burn"
   "bzeta"
   "cabs"
   "canform"
   "canten"
   "carg"
   "cartan"
   "catch"
   "cbfac"
   "cf"
   "cfdisrep"
   "cfexpand"
   "cgamma"
   "cgamma2"
   "changevar"
   "charpoly"
   "check_overlaps"
   "chr1"
   "chr2"
   "christof"
   "closefile"
   "closeps"
   "coeff"
   "coefmatrix"
   "col"
   "collapse"
   "columnvector"
   "combine"
   "comp2pui"
   "compfile"
   "compile"
   "compile_file"
   "compile_lisp_file"
   "concat"
   "conjugate"
   "cons"
   "constantp"
   "cont2part"
   "content"
   "continue"
   "contract"
   "copylist"
   "copymatrix"
   "cos"
   "cosh"
   "cot"
   "coth"
   "covdiff"
   "create_list"
   "csc"
   "csch"
   "curvature"
   "cyc"
   "dblint"
   "ddt"
   "deactivate"
   "debug"
   "debugprintmode"
   "declare"
   "declare_translated"
   "declare_weight"
   "decsym"
   "defcon"
   "define"
   "define_variable"
   "defint"
   "defmatch"
   "defrule"
   "deftaylor"
   "delete"
   "delfile"
   "delta"
   "demo"
   "denom"
   "depends"
   "derivdegree"
   "derivlist"
   "describe"
   "desolve"
   "determinant"
   "diagmatrix"
   "diff"
   "dimension"
   "direct"
   "diskfree"
   "disolate"
   "disp"
   "dispcon"
   "dispform"
   "dispfun"
   "display"
   "disprule"
   "dispterms"
   "distrib"
   "divide"
   "divsum"
   "dlt"
   "dotsimp"
   "dpart"
   "dscalar"))

(defvar maxima-match-functions-2
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-fns-2))
          "\\)\\>")
  "regexp to match the maxima functions.")

(defvar maxima-fns-3
  (list
   "echelon"
   "eigenvalues"
   "eigenvectors"
   "einstein"
   "ele2comp"
   "ele2polynome"
   "ele2pui"
   "elem"
   "eliminate"
   "ematrix"
   "endcons"
   "entermatrix"
   "entier"
   "equal"
   "erf"
   "errcatch"
   "error"
   "errormsg"
   "euler"
   "ev"
   "eval"
   "evenp"
   "example"
   "exp"
   "expand"
   "expandwrt"
   "expandwrt_factored"
   "explose"
   "express"
   "expt"
   "extract_linear_equations"
   "ezgcd"
   "factcomb"
   "factor"
   "factorial"
   "factorout"
   "factorsum"
   "facts"
   "fassave"
   "fasttimes"
   "fast_central_elements"
   "fast_linsolve"
   "featurep"
   "fft"
   "fib"
   "fibtophi"
   "filedefaults"
   "filename_merge"
   "file_type"
   "fillarray"
   "first"
   "fix"
   "float"
   "floatdefunk"
   "floatnump"
   "flush"
   "flushd"
   "flushnd"
   "forget"
   "fortmx"
   "fortran"
   "freeof"
   "fullmap"
   "fullmapl"
   "fullratsimp"
   "fullratsubst"
   "funcsolve"
   "fundef"
   "funmake"))
   

(defvar maxima-match-functions-3
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-fns-3))
          "\\)\\>")
  "regexp to match the maxima functions.")

(defvar maxima-fns-4
  (list
   "gamma"
   "gauss"
   "gcd"
   "gcfactor"
   "gendiff"
   "genfact"
   "genmatrix"
   "get"
   "getchar"
   "gfactor"
   "gfactorsum"
   "go"
   "gradef"
   "gramschmidt"
   "grind"
   "grobner_basis"
   "hach"
   "hipow"
   "horner"
   "ic1"
   "ident"
   "idummy"
   "ieqn"
   "ift"
   "ilt"
   "imagpart"
   "imetric"
   "indices"
   "infix"
   "innerproduct"
   "inpart"
   "inrt"
   "integerp"
   "integrate"
   "interpolate"
   "intopois"
   "intosum"
   "intsce"
   "invert"
   "is"
   "ishow"
   "isolate"
   "isqrt"
   "jacobi"
   "kdelta"
   "kill"
   "killcontext"
   "kostka"
   "labels"
   "lambda"
   "laplace"
   "last"
   "lc"
   "lc2kdt"
   "lcm"
   "ldefint"
   "ldisp"
   "ldisplay"
   "length"
   "let"
   "letrules"
   "letsimp"
   "levi_civita"
   "lgtreillis"
   "lhs"
   "limit"
   "linsolve"
   "lispdebugmode"
   "listarray"
   "listofvars"
   "listp"
   "list_nc_monomials"
   "load"
   "loadfile"
   "local"
   "log"
   "logcontract"
   "lopow"
   "lorentz"
   "lpart"
   "lratsubst"
   "lriccicom"
   "ltreillis"))
   

(defvar maxima-match-functions-4
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-fns-4))
          "\\)\\>")
  "regexp to match the maxima functions.")

(defvar maxima-fns-5
  (list
   "makebox"
   "makefact"
   "makegamma"
   "makelist"
   "make_array"
   "map"
   "mapatom"
   "maplist"
   "matchdeclare"
   "matchfix"
   "matrix"
   "matrixmap"
   "matrixp"
   "mattrace"
   "max"
   "member"
   "metric"
   "min"
   "minfactorial"
   "minor"
   "mod"
   "mode_declare"
   "mode_identity"
   "mon2schur"
   "mono"
   "monomial_dimensions"
   "motion"
   "multinomial"
   "multi_elem"
   "multi_orbit"
   "multi_pui"
   "multsym"
   "multthru"
   "ncexpt"
   "ncharpoly"
   "nc_degree"
   "new-disrep"
   "newcontext"
   "newdet"
   "newton"
   "niceindices"
   "nonscalarp"
   "nostring"
   "nounify"
   "nroots"
   "nterms"
   "ntermsg"
   "ntermsrci"
   "nthroot"
   "num"
   "numberp"
   "numerval"
   "numfactor"
   "nusum"
   "nzeta"
   "oddp"
   "ode"
   "ode2"
   "openplot_curves"
   "optimize"
   "orbit"
   "ordergreat"
   "ordergreatp"
   "orderless"
   "orderlessp"
   "outchar"
   "outofpois"
   "pade"
   "part"
   "part2cont"
   "partfrac"
   "partition"
   "partpol"
   "pcoeff"
   "permanent"
   "permut"
   "pickapart"
   "playback"
   "plog"
   "plot2d"
   "plot2d"
   "plot2d_ps"
   "plot3d"
   "plot3d"
   "poisdiff"
   "poisexpt"
   "poisint"
   "poismap"
   "poisplus"
   "poissimp"
   "poissubst"
   "poistimes"
   "poistrim"
   "polarform"
   "polartorect"
   "polynome2ele"
   "postfix"
   "potential"
   "powers"
   "powerseries"
   "pred"
   "prefix"
   "prime"
   "primep"
   "print"
   "printpois"
   "printprops"
   "prodrac"
   "product"
   "properties"
   "props"
   "propvars"
   "pscom"
   "psdraw_curve"
   "psi"
   "pui"
   "pui2comp"
   "pui2ele"
   "pui2polynome"
   "puireduc"
   "pui_direct"
   "put"
   "qput"
   "quit"
   "qunit"
   "quotient"))


(defvar maxima-match-functions-5
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-fns-5))
          "\\)\\>")
  "regexp to match the maxima functions.")

(defvar maxima-fns-6
  (list
   "radcan"
   "radsubstflag"
   "raiseriemann"
   "random"
   "rank"
   "rat"
   "ratcoef"
   "ratdenom"
   "ratdiff"
   "ratdisrep"
   "ratexpand"
   "ratnumer"
   "ratnump"
   "ratp"
   "ratsimp"
   "ratsubst"
   "ratvars"
   "ratweight"
   "read"
   "readonly"
   "realpart"
   "realroots"
   "rearray"
   "rectform"
   "recttopolar"
   "rem"
   "remainder"
   "remarray"
   "rembox"
   "remcon"
   "remfunction"
   "remlet"
   "remove"
   "remrule"
   "remtrace"
   "remvalue"
   "rename"
   "reset"
   "residue"
   "resolvante"
   "resolvante_alternee1"
   "resolvante_bipartite"
   "resolvante_diedrale"
   "resolvante_klein"
   "resolvante_klein3"
   "resolvante_produit_sym"
   "resolvante_unitaire"
   "resolvante_vierer"
   "rest"
   "restart"
   "restore"
   "resultant"
   "return"
   "reveal"
   "reverse"
   "revert"
   "rhs"
   "riccicom"
   "riemann"
   "rinvariant"
   "risch"
   "rncombine"
   "romberg"
   "room"
   "rootscontract"
   "row"
   "save"
   "scalarp"
   "scalefactors"
   "scanmap"
   "schur2comp"
   "sconcat"
   "scsimp"
   "scurvature"
   "sec"
   "sech"
   "setelmx"
   "setup"
   "setup_autoload"
   "set_plot_option"
   "set_up_dot_simplifications"
   "showratvars"
   "sign"
   "signum"
   "similaritytransform"
   "simp"
   "sin"
   "sinh"
   "solve"
   "somrac"
   "sort"
   "splice"
   "sprint"
   "sqfr"
   "sqrt"
   "srrat"
   "sstatus"
   "status"
   "string"
   "stringout"
   "sublis"
   "sublist"
   "submatrix"
   "subst"
   "substinpart"
   "substpart"
   "subvarp"
   "sum"
   "sumcontract"
   "supcontext"
   "sym"
   "symbolp"
   "system"
   "tan"
   "tanh"
   "taylor"
   "taylorinfo"
   "taylorp"
   "taylor_simplifier"
   "taytorat"
   "tcl_output"
   "tcontract"
   "tellrat"
   "tellsimp"
   "tellsimpafter"
   "tex"
   "throw"
   "time"
   "timer"
   "timer_info"
   "tldefint"
   "tlimit"
   "tobreak"
   "todd_coxeter"
   "toplevel"
   "totaldisrep"
   "totient"
   "to_lisp"
   "tpartpol"
   "trace"
   "trace_options"
   "transform"
   "translate"
   "translate_file"
   "transpose"
   "treillis"
   "treinat"
   "triangularize"
   "trigexpand"
   "trigrat"
   "trigreduce"
   "trigsimp"
   "trunc"
   "tr_warnings_get"
   "tsetup"
   "ttransform"
   "undiff"
   "uniteigenvectors"
   "unitvector"
   "unknown"
   "unorder"
   "unsum"
   "untellrat"
   "untrace"
   "vectorpotential"
   "vectorsimp"
   "verbify"
   "weyl"
   "writefile"
   "xthru"
   "zeroequiv"
   "zeromatrix"
   "zeta"
   "zrpoly"
   "zsolve"))


(defvar maxima-match-functions-6
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-fns-6))
          "\\)\\>")
  "regexp to match the maxima functions.")

(defvar maxima-const-1
  (list
   "%e"
   "%i"
   "%gamma"
   "%phi"
   "%pi"
   "zeroa"
   "zerob"))


(defvar maxima-match-constants-1
  (concat "\\<"
          (eval-when-compile (regexp-opt maxima-const-1))
          "\\>")
  "regexp to match the maxima constants.")

(defvar maxima-const-2
  (list
   "false"
   "ind"
   "inf"
   "infinity"
   "minf"
   "true"))

(defvar maxima-match-constants-2
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-const-2))
          "\\)\\>")
  "regexp to match the maxima constants.")

(defvar maxima-match-constants-3
  "\\<\\([0-9]+\\)\\>"
  "regexp to match the maxima constants.")

(defvar maxima-match-constants-4
  "\\<\\([0-9]+\.\\)?\\([0-9]+b[+-]?[0-9]\\)\\>"
  "regexp to match the maxima constants.")

(defvar maxima-keywds
  (list
   "allbut"))   

(defvar maxima-match-keywords
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-keywds))
          "\\)\\>")
  "regexp to match the maxima keywords.")

(defvar maxima-match-operators
  (eval-when-compile 
    (regexp-opt '(
                  "'"
                  "''"
                  "!"
                  "!!"
                  "#"
                  "."
                  ":"
                  "::"
                  "::="
                  ":="
                  "="
                  "<"
                  ">"
                  "+"
                  "-"
                  "*"
                  "/"
                  "^"
                  ) t))
  "regexp to match the maxima operators.")

(defvar maxima-props
  (list
   "atomgrad"))


(defvar maxima-match-properties
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-props))
          "\\)\\>")
  "regexp to match maxima properties.")

(defvar maxima-macros
  (list
   "buildq"
   "with_stdout"))


(defvar maxima-match-macros
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-macros))
          "\\)\\>")
  "regexp to match maxima macros.")

(defvar maxima-specops
  (list
   "do"
   "else"
   "elseif"
   "for"
   "if"
   "in"
   "step"
   "then"
   "thru"
   "unless"
   "while"))
                        

(defvar maxima-match-specops
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-specops))
          "\\)\\>")
  "regexp to match maxima special operators.")

(defvar maxima-decs
  (list
   "alphabetic"
   "antisymmetric"
   "commutative"
   "feature"
   "features"
   "lassociative"
   "linear"
   "mainvar"
   "multiplicative"
   "nonscalar"
   "noun"
   "outative"
   "posfun"
   "rassociative"
   "symmetric"))


(defvar maxima-match-declarations
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-decs))
          "\\)\\>")
  "regexp to match maxima declarations.")

(defvar maxima-spsymbs-1
  (list
   "additive"
   "constant"
   "infeval"
   "noeval"
   "nouns"
   "numer"
   "poisson"
   "verb"))


(defvar maxima-match-specsymbs-1
  (concat "\\<\\("
          (eval-when-compile (regexp-opt maxima-spsymbs-1))
          "\\)\\>")
  "regexp to match maxima special symbols.")

(defvar maxima-match-specsymbs-2
  "\\(\\<\\?\\sw+\\>\\)"
  "regexp to match maxima special symbols.")

;;; now, create the faces.

;; (defface maxima-function-face
;;   '((((type tty) (class color)) (:foreground "blue" :weight bold))
;;     (((class color) (background light)) (:foreground "blue" :weight bold))
;;     (((class color) (background dark)) (:foreground "lightskyblue" :weight bold))
;;     (t (:inverse-video t :bold t)))
;;   "font lock mode face used to highlight function names."
;;   :group 'maxima)

;; (defface maxima-constant-face
;;   '((((type tty) (class color)) (:foreground "magenta"))
;;     (((class grayscale) (background light))
;;      (:foreground "lightgray" :bold t :underline t))
;;     (((class grayscale) (background dark))
;;      (:foreground "gray50" :bold t :underline t))
;;     (((class color) (background light)) (:foreground "cadetblue" :weight bold))
;;     (((class color) (background dark)) (:foreground "aquamarine" :weight bold))
;;     (t (:bold t :underline t)))
;;   "font lock mode face used to highlight constants and labels."
;;   :group 'maxima)

;; (defface maxima-keyword-face
;;   '((((type tty) (class color)) (:foreground "cyan" :weight bold))
;;     (((class grayscale) (background light)) (:foreground "lightgray" :bold t))
;;     (((class grayscale) (background dark)) (:foreground "dimgray" :bold t))
;;     (((class color) (background light)) (:foreground "purple" :weight bold))
;;     (((class color) (background dark)) (:foreground "cyan" :weight bold))
;;     (t (:bold t)))
;;   "font lock mode face used to highlight keywords."
;;   :group 'maxima)

;; (defface maxima-operator-face
;;   '((((type tty) (class color)) (:foreground "blue" :weight light))
;;     (((class grayscale) (background light)) (:foreground "lightgray" :bold t))
;;     (((class grayscale) (background dark)) (:foreground "dimgray" :bold t))
;;     (((class color) (background light)) (:foreground "orchid" :weight bold))
;;     (((class color) (background dark)) (:foreground "lightsteelblue" :weight bold))
;;     (t (:bold t)))
;;   "font lock mode face used to highlight builtins."
;;   :group 'maxima)

;; (defface maxima-property-face
;;   '((((type tty) (class color)) (:foreground "green"))
;;     (((class grayscale) (background light)) (:foreground "gray90" :bold t))
;;     (((class grayscale) (background dark)) (:foreground "dimgray" :bold t))
;;     (((class color) (background light)) (:foreground "forestgreen" :weight bold))
;;     (((class color) (background dark)) (:foreground "palegreen" :weight bold))
;;     (t (:bold t :underline t)))
;;   "font lock mode face used to highlight type and classes."
;;   :group 'maxima)

;; (defface maxima-macro-face
;;   '((((class color) (background dark)) (:foreground "steelblue1"))
;;     (((class color) (background light)) (:foreground "blue3"))
;;     (t (:underline t)))
;;   "font lock mode face used to highlight preprocessor conditionals."
;;   :group 'maxima)

;; (defface maxima-specop-face
;;   '((((type tty) (class color)) (:foreground "cyan" :weight bold))
;;     (((class grayscale) (background light)) (:foreground "lightgray" :bold t))
;;     (((class grayscale) (background dark)) (:foreground "dimgray" :bold t))
;;     (((class color) (background light)) (:foreground "purple" :weight bold))
;;     (((class color) (background dark)) (:foreground "cyan" :weight bold))
;;     (t (:bold t)))
;;   "font lock mode face used to highlight keywords."
;;   :group 'maxima)

;; (defface maxima-declaration-face
;;   '((((type tty) (class color)) (:foreground "cyan" :weight bold))
;;     (((class grayscale) (background light)) (:foreground "lightgray" :bold t))
;;     (((class grayscale) (background dark)) (:foreground "dimgray" :bold t))
;;     (((class color) (background light)) (:foreground "purple" :weight bold))
;;     (((class color) (background dark)) (:foreground "cyan" :weight bold))
;;     (t (:bold t)))
;;   "font lock mode face used to highlight keywords."
;;   :group 'maxima)

;; (defface maxima-specsymb-face
;;   '((((type tty) (class color)) (:foreground "red"))
;;     (((class color) (background light)) (:foreground "red" :bold t))
;;     (((class color) (background dark)) (:foreground "pink" :bold t))
;;     (t (:inverse-video t :bold t)))
;;   "font lock mode face used to highlight warnings."
;;   :group 'maxima)

;; (defface maxima-variable-face
;;   '((((type tty) (class color)) (:foreground "yellow" :weight light))
;;     (((class grayscale) (background light))
;;      (:foreground "gray90" :bold t :italic t))
;;     (((class grayscale) (background dark))
;;      (:foreground "dimgray" :bold t :italic t))
;;     (((class color) (background light)) (:foreground "darkgoldenrod" :italic t))
;;     (((class color) (background dark)) (:foreground "lightgoldenrod" :italic t))
;;     (t (:bold t :italic t)))
;;   "font lock mode face used to highlight variable names."
;;   :group 'maxima)

;;; now, the keywords
(unless (facep 'font-lock-builtin-face)
  (copy-face 'font-lock-keyword-face 'font-lock-builtin-face))

(unless (facep 'font-lock-constant-face)
  (copy-face 'font-lock-keyword-face 'font-lock-constant-face))

(defvar maxima-font-lock-keywords-1
  `(
    (,maxima-match-declarations . font-lock-keyword-face))
    "Subdued level highlighting for Maxima mode.")

(defvar maxima-font-lock-keywords-2
  (append maxima-font-lock-keywords-1
   `(
     (,maxima-match-operators . font-lock-keyword-face)
     (,maxima-match-variables-1 . font-lock-keyword-face)
     (,maxima-match-variables-2 . font-lock-keyword-face)
     (,maxima-match-variables-3 . font-lock-keyword-face)
     (,maxima-match-variables-4 . font-lock-keyword-face)
     (,maxima-match-functions-1 . font-lock-builtin-face)
     (,maxima-match-functions-2 . font-lock-builtin-face)
     (,maxima-match-functions-3 . font-lock-builtin-face)
     (,maxima-match-functions-4 . font-lock-builtin-face)
     (,maxima-match-functions-5 . font-lock-builtin-face)
     (,maxima-match-functions-6 . font-lock-builtin-face)
     (,maxima-match-constants-1 . font-lock-constant-face)
     (,maxima-match-constants-2 . font-lock-constant-face)
     (,maxima-match-constants-3 . font-lock-constant-face)
     (,maxima-match-constants-4 . font-lock-constant-face)
     (,maxima-match-keywords . font-lock-keyword-face)
     (,maxima-match-properties . font-lock-keyword-face)
     (,maxima-match-macros . font-lock-keyword-face)
     (,maxima-match-specops . font-lock-keyword-face)
     (,maxima-match-specsymbs-1 . font-lock-keyword-face)
     (,maxima-match-specsymbs-2 . font-lock-keyword-face)))
  "Medium level highlighting for Maxima mode.")

(defvar maxima-font-lock-keywords-3
  (let* ((spc                 "[[:space:]]*")
	 (lspc                (concat "^" spc))
	 (name                "[%_[:alnum:]]+")
	 (fname               (concat "\\(" name "\\)"))
	 (arg                 (concat spc name spc))
	 (marg                (concat "\\(?:\\(?:" arg "," spc "\\)+" arg "\\)"))
	 (optarg              (concat spc "\\[" spc name spc "\\]" spc))
	 (1-arg               (concat spc name spc))
	 (1-arg-optarg        (concat spc name spc "," spc "\\[" spc name spc "\\]" spc)) 
	 (marg-optarg         (concat marg spc "," spc optarg))
	 (zarg                spc)
	 (fopen               (concat spc "(" spc))
	 (fclose              (concat spc ")" spc))
	 (defn                ":*:=")
	 (fbegin              (concat lspc fname fopen))
	 (fend                (concat fclose defn))
	 (alt                 "\\|")
	 ;; functions: f(...) :=
	 (fn                  (concat fbegin "\\(" zarg alt optarg alt 1-arg alt marg alt optarg alt 1-arg-optarg alt marg-optarg "\\)" fend))
	 ;; pure hash functions: f[...] :=
	 (afopen              (concat spc "\\[" spc))
	 (afclose             (concat spc "\\]" spc))
	 (afbegin             (concat lspc fname afopen))
	 (afend               (concat afclose defn))
	 (afn                 (concat afbegin "\\(" zarg alt 1-arg alt marg "\\)" afend))
	 ;; hash/ordinary functions: f[...](...) :=
	 (hfopen              (concat spc "\\[" spc))
	 (hfclose             (concat spc "\\]" spc))
	 (hfbegin             (concat lspc fname hfopen))
	 (hfend               (concat hfclose defn))
	 (hfn                 (concat hfbegin "\\(" 1-arg alt marg "\\)" hfclose fopen "\\(" zarg alt 1-arg alt marg alt optarg alt 1-arg-optarg alt marg-optarg "\\)" fend)))
    (append maxima-font-lock-keywords-2
	    (list
	     (list fn  '(1 font-lock-function-name-face))
	     (list afn '(1 font-lock-function-name-face))
	     (list hfn '(1 font-lock-function-name-face))
	     (list fn  '(2 font-lock-variable-name-face))
	     (list afn '(2 font-lock-variable-name-face))
	     (list hfn '(2 font-lock-variable-name-face))
	     (list hfn '(3 font-lock-variable-name-face)))))
  "Gaudy level highlighting for Maxima mode.")

(defvar maxima-font-lock-keywords maxima-font-lock-keywords-3
  "Default expressions to highlight in Maxima mode.")

(defun maxima-font-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
        '((maxima-font-lock-keywords maxima-font-lock-keywords-1 
           maxima-font-lock-keywords-2 maxima-font-lock-keywords-3)
          nil t)))

(add-hook 'maxima-mode-hook 'maxima-font-setup)

;;; A function to fontify the preamble in a Maxima process buffer
(defvar maxima-preamble-fontlock t)

(defun maxima-match-preamble (limit)
  "Used to fontify the preamble."
  (if maxima-preamble-fontlock
      (progn
        (setq maxima-preamble-fontlock nil)
        (let ((beg (point-min)) 
              (end))
          (if (search-forward "(%i1)" limit)
              (progn
                (forward-line -1)
                (setq end (maxima-line-end-position))
                (store-match-data (list beg end))
                t))))
    nil))

(defvar inferior-maxima-font-lock-keywords-1
  (append maxima-font-lock-keywords-1
    '((maxima-match-preamble (0 font-lock-string-face t t)))))

(defvar inferior-maxima-font-lock-keywords-2
  (append maxima-font-lock-keywords-2
    '((maxima-match-preamble (0 font-lock-string-face t t)))))

(defvar inferior-maxima-font-lock-keywords-3
  (append maxima-font-lock-keywords-3
    '((maxima-match-preamble (0 font-lock-string-face t t)))))

(defvar inferior-maxima-font-lock-keywords inferior-maxima-font-lock-keywords-3
  "Default expressions to highlight in Maxima mode.")

(defun inferior-maxima-font-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
        '((inferior-maxima-font-lock-keywords inferior-maxima-font-lock-keywords-1 
           inferior-maxima-font-lock-keywords-2 inferior-maxima-font-lock-keywords-3)
          nil t)))

;;; now for the symbols

(defvar maxima-symbols
  (mapcar
   (lambda (x) (list x))
   (append
    maxima-vars-1
    maxima-vars-2
    maxima-vars-3
    maxima-vars-4
    maxima-fns-1
    maxima-fns-2
    maxima-fns-3
    maxima-fns-4
    maxima-fns-5
    maxima-fns-6
    maxima-const-1
    maxima-const-2
    maxima-keywds
    maxima-props
    maxima-macros
    maxima-specops
    maxima-decs
    maxima-spsymbs-1)))
    
;;; end of maxima-font-lock.el
