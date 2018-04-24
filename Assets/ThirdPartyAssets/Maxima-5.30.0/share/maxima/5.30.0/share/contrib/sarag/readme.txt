---------------------------------------------------------------
---------------------------------------------------------------
                         S A R A G

       SOME ALGORITHMS IN REAL ALGEBRAIC GEOMETRY

                       Version 1.1

---------------------------------------------------------------
maintained and developed by Fabrizio Caruso 

under the scientific guidance of Marie-Francoise Roy

at the University of Rennes 1, France

with support from the RAAG network

further developed

at the University of Pisa, Italy

The code for the multivariate certificate of positivy

has been developed by Richard Leroy.

---------------------------------------------------------------

Please report bugs to: 
fabrizio.caruso@posso.dm.unipi.it
caruso@dm.unipi.it
caruso@science.unitn.it

---------------------------------------------------------------
Also part of the free interactive book

"Algorithms in Real Algebraic Geometry"

by Saugata Basu, Richard Pollack, Marie-Françoise Roy

Springer-Verlag, Berlin, 2003

which together with SARAG is available for download at 

http://name.math.univ-rennes1.fr/marie-francoise.roy/bpr-posted1.html 

---------------------------------------------------------------
---------------------------------------------------------------
THE SARAG LIBRARY
 
---------------------------------------------------------------

This is a free open source Maxima library for 
real algebraic geometry.
As of this version it focuses on 
univariate real root counting and isolation, 
topology of bivariate polynomials and 
certificate (computer-generated simple proof 
for the positivity/negativity of a univariate polynomial 
in a given interval)


In particular SARAG provides functions for 
(1) linear algebra,
(2) theory of subresultants, 
(3) Cauchy Index and its application to real root counting, 
(4) isolation of real roots,
(5) sign determination and Thom encodings,
(6) study of the topology of bivariate polynomials
(7) certificate of positivity for univariate polynomials

---------------------------------------------------------------
---------------------------------------------------------------
REQUIREMENTS

---------------------------------------------------------------

- MAXIMA (5.9.2 or above)
The library has been developed and tested 
with Maxima version 5.9.2 and 5.9.3.
SARAG with the only exception of plotting 
(e.g., "drawTopology") also works on Maxima 5.9.1.
The latest version of Maxima is available on line at
http://maxima.sourceforge.net/

- GNUPLOT (3.7.x, 4.0.x or above)
The function "drawTopology" in this library uses 
Gnuplot for plotting graphs
and it has been tested successfully on Gnuplot 3.7.x 
and 4.0.x.


---------------------------------------------------------------
---------------------------------------------------------------
LOADING THE FILES

sarag.mac
---------------------------------------------------------------

The library is contained in the following files:

sarag.mac (it loads all the files)

settings.mac (general settings)
constants.mac
sarag_initialization.mac
aliases.mac (name conventions)
lowLevel.mac (low level routines)
sarag_linear_algebra.mac (linear algebra and matrix manipulation)
rootCounting.mac (real root counting)
rootIsolation.mac (root isolation by De Casteljau method)
signDetermination.mac (sign determination)
intervalArithmetic.mac (interval arithmetic)
topology.mac (topology of curves)
certificateOfPositivity.mac (certificate of positivity)

arag_test.mac (test file for "Alg. in Real Alg. Geom.")
hard_test.mac (heavy test files with mostly topology computations)
other_test.mac (test of functions not included in the book)

readme.txt (this manual)

This library is included in the latest versions of Maxima
and it is simply loaded by the command
load(sarag);

For previous versions of the library
either load each single file
with the "LOAD" Maxima command
or edit a file that takes care of loading all the files 
or load the file "sarag.mac" that will load the files 
if they are in the current directory.

In order to test the library use:
batch(<path>/arag_test.mac,test);
and
batch(<path>/hard_test.mac,test);
and
batch(<path>/other_test.mac,test);

---------------------------------------------------------------
---------------------------------------------------------------
THE MANUAL

readme.txt
---------------------------------------------------------------

This manual describes the high level functions ("main functions")
of the SARAG library and the most important auxiliary functions.

For more details and for the theory behind the algorithms
we refer to "Algorithms in Real Algebraic Geometry"
by S. Basu, R. Pollack, M.-F. Roy
which together with SARAG is available for download at 
http://name.math.univ-rennes1.fr/marie-francoise.roy/bpr-posted1.html 


ATTENTION: 

(*) Items marked with this symbol are not fully tested
or are missing some minor features.

(**) These items are not available, yet, but will
be included soon.

---------------------------------------------------------------
---------------------------------------------------------------
SETTINGS

settings.mac
---------------------------------------------------------------

The library contains some settings in the file "settings.mac".

The main settings and their default values are:

- DEFAULT_VERBOSITY [0 (non-verbose)] (**)
Verbosity level of the commands.

- LINEAR_SOLVER [linsolve (built-in Maxima linear solver)]
Solver of linear systems used in some routines.

- MOD_TEST_PRIME [2]
Prime used in some modular tests.

- NORM_ALGORITHM [lambda([x],ratexpand)]
Normal form computed in "sSubRes" and related algorithms

- ASSUME_EXPANDED [false]
Assunption on whether the polynomial input of the main functions
is in expanded form

- WARNINGS [true]
Warnings

- ASSUME_GENERIC_POSITION  [false];
Assumption on the generic position of curves 

- PLOT_STYLE ["set noxtics; set noytics; set nokey;"]
Preamble to be fed to gnuplot to set its style

- PS_OUTPUT  [false]
Printable file output for "drawTopology"

- PS_OUTPUT_FILE_NAME  ["test.eps"]
Name of the postscript file output for "drawTopology"


For the other settings we refer to file 
settings.mac

---------------------------------------------------------------
---------------------------------------------------------------
NAMING CONVENTIONS

aliases.mac
---------------------------------------------------------------

The names of main functions are formed by adding
prefixes to specify the method/algorithm to be used and 
suffixes to specify an alternative version of the
function or output.
Auxiliary functions may not follow such conventions.

Therefore the general name convention for the names 
of the high level functions is the following:

METHOD | WHAT IS TO BE COMPUTED | MODIFIER

When a prefix is added capitalization 
of the function is used to distinguish the
parts of the new name.

When no prefix or no suffix is used the
default version of function with the
default method/algorithm will be called
as set in the file: "aliases.mac"

NOTE:
In this manual some possible modifiers will appear
in brackets.

Examples:

tarskiQuery [computes the Tarski query over all R with
the default algorithm]

sRemTarskiQueryBetween [computes the Tarski query
over an interval using the signed remainder sequence]

det [computes the determinant of a matrix with the
default algorithm]

bareissDet [computes the determinant of a matrix
by using Bareiss method]

sSubRes [computes the signed subresultant sequence]

sSubResExt [computes the extended signed subresultant sequence]


---------------------------------------------------------------
---------------------------------------------------------------
COMMANDS
---------------------------------------------------------------

Here we describe the most important functions:
all the "main functions" and some 
important "auxiliary functions".

REMARK: 
Our naming conventions and the
global variables DEFAULT_VERBOSITY and ASSUME_EXPANDED
only apply to "main functions".

When multiple methods are available for a 
main function  we list all of them.
They are accessible by adding the name 
of the method as a prefix and by capitalizing
the first letter of the name of the function.
For more details on names of functions read
the section on naming conventions.


ATTENTION:

-------------------------------------------------
Polynomial Input in Expanded Form

If ASSUME_EXPANDED is set to TRUE then
whenever the input of a main function contains polynomials
we are always assuming that they are in EXPANDED FORM.

Independently on the value of ASSUME_EXPANDED 
we will always assume that the polynomial
input of auxiliary functions is in EXPANDED FORM.

Therefore, in these cases,
when defining a polynomial use for instance:

p : expand(...);

-------------------------------
Form of the Output

The output of all functions of the library is a Maxima expression. 
Maxima uses brakets "[", "]" to describe couples and lists
(e.g. an open inteval ]a,b[ is described by a couple
containing the ends, which in Maxima is "[a,b]")


---------------------------------------------------------------
LINEAR ALGEBRA

sarag_linear_algebra.mac
---------------------------------------------------------------

This file contains functions related to Gaussian elimination
and matrix manipulations.

---------------------------------------------------------------
Auxiliary functions


- matrixProd(a,b)
INPUT : a and b are lists of lists representing the rows 
of two matrices such that a has the same number of columns
as b as rows
OUTPUT : the row-column product matrix of a, b

---------------------------------------------------------------
Main functions

- det(m)
INPUT : m is a list of lists representing the rows of a matrix 
METHOD : 
-- gauss [Gaussian elimination with no pivot optimization], 
-- bareiss [Bareiss method]
OUTPUT : the determinant


- elim(m) 
INPUT : m is a list of lists representing the rows of a matrix
METHOD : 
-- gauss [Gaussian elimination with no pivot optimization], 
-- bareiss [Bareiss method] (**)
OUTPUT : [t,c,z]
where 
1) t is an upper triangular equivalent form of m
with possibly some zero rows computed by Gaussian
elimination with divisions and columns exchanges
2) c is the list of couples describing the columns
exchanges
3) z is the list of zero rows


-----------------------------------------------------------------
CHARACTERISTIC POLYNOMIAL

sarag_linear_algebra.mac
-----------------------------------------------------------------

-----------------------------------------------------------------
Auxiliary functions

- getCoeffFromNewton(ns) 
INPUT : a list containing the the Newton sums of
a given polynomial
OUTPUT : an array containing the coefficients of the
corresponding polynomial

-----------------------------------------------------------------
Main functions

- charPol(A,var)
INPUT : A is a list of lists representing the rows of a matrix,
an indeterminate var
METHOD : 
-- gauss [Gaussian elimination with no pivot optimization], (**) 
-- bareiss [Bareiss method], (**)
-- babyGiant [baby step, giant step trace-based method]
OUTPUT : the characteristic polynomial of A in the indeterminate var


-----------------------------------------------------------------
SIGNED REMAINDER SEQUENCE

rootCounting.mac
-----------------------------------------------------------------

-----------------------------------------------------------------
Main functions

- sRem(a,b,x)
INPUT : a, b polynomials in the x indeterminate
OUTPUT : list containing signed remainder sequence

- sRemExt(a,b,x)
INPUT : a, b polynomials in the x indeterminate
OUTPUT : [r,u,v] forming the extended signed remainder sequence 
where r is the signed remainder sequence and u and v are
the corresponding sequences of cofactors

-----------------------------------------------------------------
SIGNED SUBRESULTANTS

rootCounting.mac
-----------------------------------------------------------------

-----------------------------------------------------------------
Main functions

- sSubRes(a,b,var)
INPUT : a,b polynomials in the x indeterminate with deg(a)>deg(b)
OUTPUT : [sSubRes,s] where 
1) sSubRes is a list containing the signed subresultant sequence
2) s is the list of the corresponding coefficients 


- sSubResExt(a,b,var) 
INPUT : a,b polynomials in the x indeterminate with deg(a)>deg(b)
OUTPUT : [sSubRes,s,u,v] where
1) sSubRes is a list containing the signed subresultant sequence
2) s is the list of the corresponding coefficients
3) u, v are the corresponding cofactors

--------------------------------------
GCD and GCD-free part by subresultants

- gcdFreePart(P,Q,var) (*)
INPUT : polynomials P,Q in var
OUTPUT : a couple [g,f]
where g is gcd(P,Q) and
f is the gcd-free part of P with respect to Q,
both the gcd and the gcd-free are considered up
to a constant multiple

-----------------------------------------------------------------
ROOTS COUNTING

rootCouting.mac
rootIsolation.mac (for De Casteljau-based's method) 
-----------------------------------------------------------------


-----------------------------------------------------------------
Auxiliary functions

- sturmSequence(p,x)
INPUT : polynomial p in the x indeterminate
OUTPUT : the Sturm sequence of p

-----------------------------------------------------------------
Main functions


----------------------------------------------
Cauchy Index on the real line

- cauchyIndex(q,p,x)
INPUT : q,p polynomials in the x indeterminate,
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
OUTPUT : Cauchy index of q/p in the whole real line

- tarskiQuery(q,p,x)
INPUT : q,p polynomials in the x indeterminate,
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
OUTPUT : Tarski query of q,p in the whole real line

- numberOfRoots(p,x)
INPUT : p polynomials in the x indeterminate
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
-- deCasteljau [De Casteljau method for isolation]
-- monomial [monomial method for isolation] (**)
OUTPUT : number of real roots of p

--------------------------------------------------
Cauchy Index on an open interval

Remark: Here we assume that a and b are not roots


- cauchyIndexBetween(num,den,x,a,b)
INPUT : num,den polynomials in the x indeterminate,
a,b are either real or -INFINITY or +INFINITY
METHOD : 
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
OUTPUT : Cauchy index of num/den in the interval ]a,b[

- tarskiQueryBetween(q,p,x,a,b)
INPUT : q,p polynomials in the x indeterminate,
a,b are either real or -INFINITY or +INFINITY
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
OUTPUT : Tarski query of  q,p in ]a,b[

- numberOfRootsBetween(p,x,a,b)
INPUT : p polynomials in the x indeterminate,
a,b are either real or -INFINITY or +INFINITY
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
-- deCasteljau [De Casteljau method for isolation] (**)
-- monomial [monomial method for isolation] (**)
OUTPUT : number of real roots of p in the interval ]a,b[

-----------------
Hankel Signature

- hankelSignature(seq) (*)
INPUT : sequence seq of odd length of elements of an integral domain
OUTPUT : signature of the Hankel quadratic form for seq

-----------------
Complex Roots with positive/negative real part

- posNegDiff(p,x)
INPUT : polynomial p in x
OUTPUT : difference between the number of roots of p with
positive real part and those with negative real part

-----------------
Bezoutian related

- bez(p,q,var,x,y)
INPUT : p,q polynomials in var, variables x,y
OUTPUT : Bezoutian of p,q with respect to x,y

-----------------------------------------------------------------
ISOLATION OF ROOTS

rootIsolation.mac
-----------------------------------------------------------------

The routines contained in this files deal with
the problem of isolation of real roots by
using the conversion to the Bernstein basis
and De Casteljau's method.

-----------------------------------------------------------------
Auxiliary functions

--------------------------
Cauchy Bounds

- cauchyRootUpperBound(pol,x)
INPUT : polynomial pol in x
OUTPUT : upper bound for the absolute values of all its real roots

- cauchyRootLowerBound(pol,x)
INPUT : polynomial pol in x
OUTPUT : lower bound for the absolute values of 
all its non-zero real roots

- primeCauchyRootUpperBound(pol,x)
INPUT : polynomial pol in x
OUTPUT : (alternative) upper bound for the 
absolute values of all its real roots

- primeCauchyRootLowerBound(pol,x)
INPUT : polynomial pol in x
OUTPUT : (alternative) lower bound for the 
absolute values of all its non-zero real roots

---------------------------
Bernstein Basis

- convert2Bernstein(p,d,x,l,r) 
INPUT : polynomial p in the x indeterminate of degree at most d, parameters l,r
OUTPUT : list containing the coefficients of p in the
Bernstein basis of degree d for l,r

- bernsteinCoeffList(p,x,l,r) 
INPUT : polynomial p  in the x indeterminate, parameters l,r
OUTPUT : list containing the coefficients of p in the
Bernstein basis of degree deg(P) for l,r

- bernsteinSplit(coeffList, l,r,m)
INPUT : list coeffList containing the coefficients of a polynomial
in the Bernstein basis of degree d for l,r
parameters l,r,m
OUTPUT : [bern_lm, bern_mr] 
where
1) bern_lm is a list containing the coefficients
in the Bernstein basis of degree d for l,m and
2) bern_mr is a list containing the coefficients
in the Bernstein basis of degree d for m,r

- specialBernsteinSplit(coeffList,l,r)
INPUT : list coeffList containing the coefficients of a polynomial P
in the Bernstein basis of degree deg(P) for l,r,
parameters l,r
OUTPUT : [bern_first,bern_second]
where
bern_first is a list containing the coefficients
in the Bernstein basis for l,(l+r)/2 of 2^deg(P) P
bern_second is a list containing the coefficients
in the Bernstein basis for (l+r)/2,r of 2^deg(P) P

-----------------------------------------------------------------
Main functions

- isolateRoots[withZ](pol,x)
INPUT : polynomial pol in x
METHOD:
-- deCasteljau [De Casteljau method for root isolation]
-- monomial [root isolation in the monomial basis] (**)
MODIFIER: 
-- withZ : it only computes integer Bernstein coefficients
OUTPUT : list of elements  
of the form either
a) [pt]
describing the real root pt 
or 
b) [a,b]
describing the open interval "]a,b["   


- isolateRootsBetween[withZ](pol,x,search_interval)
INPUT : polynomial pol in x, the open interval search_interval
METHOD:
-- deCasteljau [De Casteljau method for root isolation]
-- monomial [root isolation in the monomial basis] (**)
MODIFIER: 
-- withZ : it only computes integer Bernstein coefficients
OUTPUT : list of elements
describing roots in the interval search_interval  
of the form either
a) [pt]
describing the real root pt 
or 
b) [a,b]
describing the open interval "]a,b["  




- findRoots[withZ](pol,x,threshold)
INPUT : polynomial pol in x, theshold for the intervals
METHOD:
-- deCasteljau [De Casteljau method for root isolation]
-- monomial [root isolation in the monomial basis] (**)
MODIFIER: 
-- withZ : it only computes integer Bernstein coefficients 
OUTPUT : list of elements  
of the form either
a) [pt]
describing the real root pt 
or 
b) [a,b]
describing the open interval "]a,b[" smaller then threshold


- findRootsBetween[withZ](pol,x,threshold)
INPUT : polynomial pol in x, theshold for the intervals
METHOD:
-- deCasteljau [De Casteljau method for root isolation]
-- monomial [root isolation in the monomial basis] (**)
MODIFIER: 
-- withZ : it only computes integer Bernstein coefficients 
OUTPUT : list of elements describing roots in the 
open interval search_interval
of the form either
a) [pt]
describing the real root pt 
or 
b) [a,b]
describing the open interval "]a,b[" smaller then threshold


- rootsSign(isInt,p,q,x)
INPUT : polynomials p,q in x,
isolating list isInt for the real roots of p in the
same form as in the output of "isolateRealRoots"
METHOD:
-- deCasteljau [De Casteljau method for root isolation]
-- monomial [root isolation in the monomial basis] (**)
OUTPUT : [[nCPtList,nCIntList],[cPtList,cIntList]]
where 
1) nCPtList, cPtList are lists of couples
describing a real root and the sign of q
at this real root
2) nCIntList,cIntList are lists of couples
describing an open interval containing exactly one 
the real-root of p, and describing the sign of q
at this root

- compareRoots(p,q,x)
INPUT : polynomials p,q in x
METHOD:
-- deCasteljau [De Casteljau method for root isolation]
-- monomial [root isolation in the monomial basis] (**)
OUTPUT : [com,signNComP,signNComQ]
where
1) com is an isolating list for the common real roots
of p and q
2) signNComP is an isolating list with signs of q for the
real roots of p that are not roots of q
3) signNComQ is an isolating list with signs of p for the
real roots of q that are not roots of p
Remark: all these one intervals and points isolate
the union of the zeros of p and q, i.e. also have
empty intersection among themselves.

-----------------------------------------------------------------
SIGN DETERMINATION

signDetermination.mac
-----------------------------------------------------------------

NOTE: When we refer to an algorithm that computes the
Tarski query of polynomials we assume that it has
the same input/output format as
"tarskiQuery" (see "ROOT COUNTING")

-----------------------------------------------------------------
Auxiliary functions

- matrixOfSigns(adaExpList,signCondList)
INPUT : list adaExpList of t-uples of exponents,
list of of t-uples signs (-1,0,1), where adaExpList is adapted to sign
determination for a set of t elements (polynomials) on signCondList
OUTPUT : the corresponding matrix of signs

- tarskiQueryVector(adaExpList,polList,var,SQ,ptSet)
INPUT : list adaExpoList of t-uples of exponents,
list of polynomials in var, 
description of a finite set of points ptSet,
algorithm SQ to compute
the Tarski query with respect to ptSet
OUTPUT : vector containing the Tarski Queries 
with respect to ptSet of 
the polynomials in polList with exponents given
by adaExpList

-----------------------------------------------------------------
Main functions

- signDetermination(polList,ptSet,sqAlg,var)
INPUT : list polList of polynomials in var, 
a description of a finite set of points,
an algorithm sqAlg to compute the Tarski query of polynomials
in polList
with respect to ptSet
METHOD :
-- naive [brute force method on a huge matrix of signs]
-- smart [a method that uses much smaller matrices of signs]
OUTPUT : a list representing 
a subset of  the all elements of {0,-1,1}^polList
describing the possible signs of the polynomials 
in polList at ptSet


-----------------------------------------------------------------
THOM ENCODINGS

signDetermination.mac
-----------------------------------------------------------------

Note: Here we refer to "extended Thom-encoding" for P with 
respect to Q as to the sign determination for Q, all the
derivatives of Q, all the derivatives of P at the roots of P

-----------------------------------------------------------------
Auxiliary functions

- thomLess(lhs,rhs)
INPUT : Thom encodings lhs, rhs
OUTPUT : TRUE if lhs<rhs, FALSE otherwise

- thomSort(thomList) 
INPUT : list of Thom-encodings
OUTPUT : order list of Thom-encodings

- extThomEncoding(P,Q,var) 
INPUT : polynomials P,Q in var
OUTPUT : a list of "extended Thom-encodings" for P w.r.t. Q,

- extThomMerge(lhs,lhsDeg,rhs,rhsDeg) 
INPUT : lhs, rhs are "extended Thom-encodings" for two polynomials
one with respect to the other,
lhsDeg, rhsDeg are the degrees of the polynomials
OUTPUT : a list containing
elements of the form [owner,thomInf] with
the following possibilities
a) [0,thomP,thomQ]
for a common root of P
with Thom encoding thomP 
and of Q with Thom encoding thomQ
b) [1,thomP] 
for a root of P not of Q with Thom encoding thomP
c) [2,thomQ] 
for a root of Q not of P
with Thom encoding thomQ
-----------------------------------------------------------------
Main functions

- thomEncoding(P,var) 
INPUT : polynomial P in var
OUTPUT : a list containing the Thom encoding of the real roots of P

- thomCompare(P,Q,var) 
INPUT : polynomials P,Q in var
OUTPUT : a list containing
elements of the form [owner,thomInf] with
the following possibilities
a) [0,thomP,thomQ]
for a common root of P
with Thom encoding thomP 
and of Q with Thom encoding thomQ
b) [1,thomP] 
for a root of P not of Q with Thom encoding thomP
c) [2,thomQ] 
for a root of Q not of P
with Thom encoding thomQ


- thomSignsAtRoots(P,Q,var) 
INPUT : polynomials P,Q in var
OUTPUT : a list of couples containing
for each root of P its Thom encodings
and the corresponding sign of q
  

-----------------------------------------------------------------
TOPOLOGY

topology.mac
-----------------------------------------------------------------

NOTE: When we refer to an algorithm for isolating real roots
in an archimedian real closed field we assume 
that has the same input/output format as
"isolateRoots" (see ROOT ISOLATION).

-----------------------------------------------------------------
Main functions

- archimedianTopology(P,isolAlg,x,y)
INPUT : a square free polynomial P in x and y, 
algorithm "isolAlg" for the
isolation of real roots in an archimedian real closed field
OUTPUT : a couple containing 

i) the number "a" of changes of 
system of coordinates in order to obtain a 
curve in generic position, i.e. we assume that the
system has been changed by x -> x+a*y,

ii) the topology with respect to the x-axis
of the curve, which is represented as a sequence
alternating numbers and couples 
where 

ii.1) the numbers describe the number of intersections
of the curve with projections to the x-axis in intervals
between critical points

ii.2) the couples contain:
ii.2.1) the number of intersections
with projections to the x-axis on critical points
ii.2.2) the position of the critical on the projection
(in bottom-up order).


-drawTopology(tpg) (*)
INPUT : the topology of the curve (as in the second
element of the output of archimedian topology)
OUTPUT : number of critical points 
EFFECT: it uses gnuplot (3.7.x, 4.0.x or above) to draw the topological graph
corresponding to description in tpg


-----------------------------------------------------------------
INTERVAL ARITHMETIC

intervalArithmetic.mac (*)
-----------------------------------------------------------------

- evaluatePolAt(P,var,interval) 
INPUT : polynomial P in var, an interval describing a root of P
OUTPUT : an interval describing the value of P at the root
described by the input

-----------------------------------------------------------------
CERTIFICATE OF POSITIVITY

certificateOfPositivity.mac
-----------------------------------------------------------------

Note:

[0] A certificate is an elementrary proof of the positivity
of a given polynomial in a given interval.

[1] A certificate of positivity/negativity for a polynomial 
P in a given interval R is 
a LIST of :
[<sub-interval of R>, C, <Bernstein basis of C P>]
where the sub-intervals cover entirely R.


[2] The output of the main functions for a given polynomial P 
are of the following form:

(a) When the polynomial P has a root in the considered interval:
[ 0, 
<interval [a,b] such that Q(a)Q(b)<0 for a polynomial divisor Q>, 
<A polynomial divisor Q of P>]

(b) When the polynomial P is positive/negative in the given interval:
[ 1/-1, 
<list of certificates covering the considered interval>] 


-----------------------------------------------------------------

Auxiliary functions

- sqFreeCertificate(pol,var)

INPUT : square-free polynomial in var
MODIFIER :sqFreeCertificateBetween(pol,var,search_interval)
OUTPUT : list of local "certificates" of positivity/negativity
that cover the default interval ([-1]).


- bernsteinMerge(lhsBern, rhsBern)

INPUT : certificates lhsBern, rhsBern
OUTPUT : a certificate for the interval covering both lhsBern and rhsBern 


- compressCertificate(cert_list)

INPUT  : a list of certificates
OUTPUT : a possibly shorter and equivalent list of certificates  

-----------------------------------------------------------------
Main functions


- certificate(pol,var)
INPUT : polynomial in var
MODIFIER : certificateBetween
OUTPUT : list of local "certificates" of positivity/negativity
that cover entirely the default interval ([-1,1]).


- certificateProof(pol,var,search_interval)
INPUT : a polynomial in var, a search interval (ex. [-1,-1])         
OUTPUT : number of subintervals used for proving the positivity/negativity 
EFFECT : it prints a formal proof of positivity/negativity of pol in    
search_interval or of the existence of a root


-----------------------------------------------------------------
MULTIVARIATE CERTIFICATE OF POSITIVITY

multivariateCertificateOfPositivity.mac
-----------------------------------------------------------------

- multiCertificate(P,V,vars,d,sub,cert)
INPUT: 
polynomial P in vars,
simplex in V, 
list of variables "vars",
degree d,
subdivision algorithm "sub",
type of certificate "cert"
EXAMPLE:
f:9*y^2-24*x*y+12*y+16*x^2-16*x+5;
Cf:certificate(f,[[0,0],[1,0],[0,1]],[x,y],2,bisection,pos);

- (wx)drawMultiCertificate(C,sc)
INPUT: output of "multiCertificate", scale sc
EFFECT: it draws the subdivision corresponding to the certificate C
