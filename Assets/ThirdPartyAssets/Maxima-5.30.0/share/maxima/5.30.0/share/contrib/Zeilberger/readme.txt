--------------------------------------------------------------------------
ZEILBERGER (Version 4.0)
by Fabrizio Caruso
--------------------------------------------------------------------------
Implementation Zeilberger's algorithm for
definite hypergeometric summation and of 
Gosper's algorithm for indefinite hypergeometric
summation in the Maxima computer algebra system.

The package also uses Axel Riese's optimization ("Filtering").

This version of the package has been tested with 5.9.3 of Maxima.
--------------------------------------------------------------------------

This package was developed by Fabrizio Caruso

[Version 1.0] (~1999)
at RISC-Linz, 
J.Kepler Universitaet (Austria) 

[Version 2.0] (2004)
at Dipartimento di Matematica "L. Tonelli", 
Università di Pisa (Italy) 

[Version 3.0] (2005)
at IRMAR, 
Université de Rennes 1.

[Version 4.0] (June 2006) 
at  Dipartimento di Matematica "L. Tonelli",
Università di Pisa (Italy) 

--------------------------------------------------------------------------
DESCRIPTION OF THE PROBLEMS
--------------------------------------------------------------------------

THE INDEFINITE PROBLEM

The package provides an implementation of Gosper's algorithm
for indefinite hypergeometric summation:

Given a hypergeometric term F_k in k we want to find its hypergeometric
anti-difference, i.e. a hypergeometric term f_k such that:

F_k = f_(k+1) - f_k.


--------------------------------------------------------------------------

THE DEFINITE PROBLEM

The package provides an implementation of Zeilberger's algorithm
for definite hypergeometric summation:

Given a proper hypergeometric term (in n and k) F_(n,k) and a
positive integer d we want to find a d-th order linear
recurrence with polynomial coefficients (in n) for F_(n,k)
and a rational function R in n and k such that:

a_0 F_(n,k) + ... + a_d F_(n+d),k = Delta_K(R(n,k) F_(n,k)),

where

Delta_k is the k-forward difference operator, i.e.,
Delta_k(t_k) := t_(k+1) - t_k.

--------------------------------------------------------------------------
LOADING
--------------------------------------------------------------------------

In order to load the package 
either load all the ".mac" files manually or 
first edit the line of "zeilberger.mac" defining 
the directory ("dir") containing the
package files, then load "zeilberger.mac" 
which will take care of loading and evalutating 
all the necessary files.

--------------------------------------------------------------------------
INSTRUCTIONS
--------------------------------------------------------------------------

- AntiDifference(F_k,k)

AntiDifference either finds the hypergeometric anti-difference
of F_k if it exists, or if it does not exist, it outputs:
NO_HYP_ANTIDIFFERENCE

If the input is not hypergeometric it outputs:
NON_HYPERGEOMETRIC
--------------------------------------------------------------------------

- Gosper(F_k,k)

Gosper either finds the rational certificate R(k) for F_k, i.e.
a rational function such that:

F_k = R(k+1) F_(k+1) - R(k) F_k
 
if it exists, or if it does not exist, it outputs:
NO_HYP_SOL

If the input is not hypergeometric it outputs:
NON_HYPERGEOMETRIC
---------------------------------------------------------------------------

- GosperSum(F_k,k,a,b) 

GosperSum either sums F_k over the interval [a,b] if
F_k has a hypergeometric anti-difference, or 
if it does not have it, it outpus:
NONGOSPER_SUMMABLE

If the input is not hypergeometric it outputs:
NON_HYPERGEOMETRIC
---------------------------------------------------------------------------

- parGosper(F_n,k,n,d)

parGosper tries to find a d-th order recurrence.
If it finds one it outputs it in the form (*).
If it finds none it outputs the empty solution [].

If the input is not proper hypergeometric in k and n it outputs:
[NON_PROPER_HYPERGEOMETRIC]
---------------------------------------------------------------------------

- Zeilberger(F_n,k,n)

- Zeilberger(F_n,k,n,d) [same as "parGosper(F_n,k,n,d)"]

Zeilberger starts by invoking "Gosper" and if it fails
tries "parGosper" with order 1 and tries up to 
the order given by the variable MAX_ORD.
If Zeilberger finds a solution before reaching MAX_ORD
it stops and yields the solution in the form (*) otherwise
it tries a higher oder.
If no solution is found it outputs [].

If the input is not proper hypergeometric in k and n it outputs:
[NON_PROPER_HYPERGEOMETRIC]

Remark: "Gosper" is skipped if the setting
GOSPER_IN_ZEILBERGER is set to FALSE.
--------------------------------------------------------------------------

(*)
FORM OF THE OUTPUT OF "parGosper" AND "ZEILBERGER"

The algorithms yields a sequence:
[s_1,s_2, ..., s_m] of solutions.

Each solution has the following form:

[R(n,k), [a_0, a_1, ..., a_d]] 

---------------------------------------------
AUTHOMATIC PROVERS OF RECURRENCES/IDENTITIES


Starting with version 4.0, I have included some routines
that produce formal simple human-readable out of the 
output of "Gosper", "parGosper" and "Zeilberger".
This is very simple becase the output contains a 
so-called "rational certificate" 

Level of details in the proofs and test:
The level of details of the proves depend on the
variables "gs_prove_detail" and "zb_prove_detail",
whose values can range in the set:
{PROOF_SILENT,PROOF_LOW,PROOF_MEDIUM,PROOF_HIGH}.


- gs_prove(F_k,k,cert)

It produces a proof for the output "cert" of "Gosper(F_k,k)",
with level of details depending on the value of "gs_prove_detail".

---------------------------------------------------------------------------

- zb_meaning(F_n,k,n,zb_cert,zb_rec)

It spells out the meaning of one of the elements 
in the output of "parGosper" or "Zeilberger", i.e. 
something of the form "[zb_cert,zb_rec]"

---------------------------------------------------------------------------

- zb_prove(F_n,k,n,zb_out)

- zb_prove(F_n,k,n) [zb_out is computed by "Zeilberger"]

It writes a proof and DOES A TEST for all the solutions
contained in "zb_out", with level of details depending on the
value of "zb_prove_detail".
"zb_out" must have the same for as the output of
"parGosper" and "Zeilberger".

For example:
"
%i30) res : parGosper(binomial(n,k),k,n,1);
				  k
(%o30) 			    [[---------, [2, - 1]]]
			      n - k + 1
(%i31) zb_prove_detail:PROOF_HIGH;
(%o31) 				       3
(%i32) zb_prove(binomial(n,k),k,n,res);
The result contains one recurrence for  binomial(n, k) :  
  
2 binomial(n, k) - binomial(n + 1, k)  =  
(k + 1) binomial(n, k + 1)   k binomial(n, k)
-------------------------- - ---------------- ; 
	  n - k		        n - k + 1
  
which we can prove by dividing both members of the equality by  binomial(n, k) 
and checking the resulting equality between rational functions. 
Namely it is equivalent to test the equality between:  
      n + 1		    k
2 - ---------  and  1 - --------- 
    n - k + 1	        n - k + 1
  
(%o32) 				     true
"

---------------------------------------------------------------------------

- zb_test(zb_in_lst)

It does a test for a list containing 
possible inputs for "parGosper", i.e. 
each element of the list is a quadruple 
of the form: "[F_n,k,k,n,order]".
The level of details shown depend on "zb_prove_detail".

Remark: 
The new version of the package includes some tests for "zb_test"
that contain examples for both "Gosper" and "parGosper" most of which
come from real mathematical problems.
For example, if you have time 
try 
"zb_test(FULL_TEST)"
or if you have less time
"zb_test(EASY_TEST)".


--------------------------------------------------------------------------
SETTINGS
--------------------------------------------------------------------------

The package provides many settings set by the
following variables whose default values are defined
in the file "settings.mac".

--------------------------------------------------------------------------
General settings

max_ord : 
maximum order used by Zeilberger [5]

simplified_output : 
further simplification of the solution [FALSE]

linear_solver : 
which solver is used to solve the system
in Zeilberger's algorithm [linsolve]

warnings : 
warnings during execution [TRUE]

gosper_in_Zeilberger : 
"Zeilberger" tries "Gosper" [TRUE]

trivial_solutions :  
Solutions by Zeilberger's algorithm
involving zero certificate or all identically zero coefficients
are also output [TRUE]

--------------------------------------------------------------------------
Settings related to the provers and test routines 

The values of the details range in 
{PROOF_SILENT,PROOF_LOW,PROOF_MEDIUN,PROOF_HIGH}.

gs_prove_detail : [PROOF_MEDIUM]
Level of details in "gs_prove"

zb_prove_detail : [PROOF_LOW]
Level of details in "zb_prove" and "zb_test"

--------------------------------------------------------------------------
Settings related to the modular test

mod_test : 
modular test for discarting systems with no solutions 
in "parGosper" and "Zeilberger" [FALSE]
 
modular_linear_solver :
linear solver used by the modular test [linsolve]

ev_point : 
evaluation point at which the variable "n" is evaluated
when performing the modular test [BIG_PRIMES[10]]

mod_big_prime : 
modulo used by the modular test [BIG_PRIMES[1]]

mod_threshold : 
threshold for the order for which a modular test
can be used [4]

--------------------------------------------------------------------------
VERBOSITY LEVELS
--------------------------------------------------------------------------

There are also verbose versions of the commands
which are called by adding one of the following prefixes:

"Summary" : just a summary at the end is shown
"Verbose" : some information in the intermidiate steps
"VeryVerbose" : more information
"Extra" : even more information including information on
the linear system in Zeilberger's algorithm


For example: "GosperVerbose", "parGosperVeryVerbose",
"ZeilbergerExtra", "AntiDifferenceSummary".

