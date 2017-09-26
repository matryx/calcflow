.TL
GENTRAN USER'S MANUAL
.sp
- VAXIMA VERSION -
.sp
* Draft *
.AU
Barbara L. Gates
.AI
Department of Mathematical Sciences
Kent State University
Kent, Ohio  44242
.sp
December 1985
.PP
.DS C
.ft CR



\fIABSTRACT\fR
.ft
.DE
.LP
GENTRAN is an automatic code GENerator and TRANslator which runs
under VAXIMA and REDUCE.  It constructs complete numerical
programs based on sets of algorithmic specifications and symbolic
expressions.  Formatted FORTRAN, RATFOR or C code can be generated
through a series of interactive commands or under the control of a template
processing routine.  Large expressions can be optimized and
segmented into subexpressions of manageable size, and a special
file-handling mechanism maintains stacks of open I/O channels to allow
output to be sent to any number of files simultaneously and to facilitate
recursive invocation of the whole code generation process.  GENTRAN
provides the flexibility necessary to handle most code generation
applications.
.LP
This manual describes the usage of the GENTRAN package for VAXIMA.
