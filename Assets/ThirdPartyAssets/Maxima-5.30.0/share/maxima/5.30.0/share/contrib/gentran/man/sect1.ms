.NH 1
INTRODUCTION
.LP
Solving a problem in science or engineering is often a two-step
process.  First the problem is modeled mathematically and derived
symbolically to provide a set of formulas which describe how to
solve the problem numerically.  Next numerical programs are
written based on this set of formulas to efficiently
compute specific values for given sets of input.  Computer algebra
systems such as MACSYMA /9,10/ provide powerful tools for use in the
formula-derivation phase but only provide primitive program-coding
tools.  The GENTRAN package has been constructed to automate
the tedious, time consuming and error-prone task of writing numerical
programs based on a set of symbolic expressions.
.NH 2
The GENTRAN Code Generator and Translator
.LP
The GENTRAN code GENeration and TRANslation package /4,5/, originally
implemented in Franz LISP /3,14/ to run under VAXIMA /2/, has also been
implemented in RLISP to run under REDUCE /6/.  Although GENTRAN
was originally created specifically to generate numerical
code for use with an existing FORTRAN-based finite element analysis
package /12,13/, it was designed to provide the flexibility
required to handle most code generation
applications.  GENTRAN contains code generation functions, file-handling
functions, mode switches, and global variables, all of which are
accessible from both the user level and the LISP level of VAXIMA to
give the user maximal control over the code generation process.  Formatted
FORTRAN /1/, RATFOR /7/ or C /8/ code can be generated from
algorithmic specifications,
i.e., descriptions of the behavior of the target numerical program expressed
in the VAXIMA programming language, and from symbolically derived expressions
and formulas.
.LP
In addition to arithmetic expressions and assignment statements,
GENTRAN can also generate type declarations and control-flow
structures.  Code generation can be guided by user-supplied
template file(s) to insert generated code into pre-existing program
skeletons, or it can be accomplished interactively through a series
of translation commands without the use of template files.  Special
mode switches enable the user to turn on or off specific features such as
automatic segmentation of large expressions, and global variables
allow the user to modify the code formatting process.  Generated
code can be sent to one or more files and, optionally, to
the user's terminal.  Stacks of open I/O channels facilitate temporary
output redirection and recursive invocation of the code generation process.
.NH 2
Organization of the Manual
.LP
The remainder of this manual is divided into five sections.  Sections
2 and 3 describe code generation.  Section 2 explains interactive code
generation, the expression segmentation facility, code optimization,
and how temporary
variables can be generated; then section 3 explains how code generation
can be guided by a user-supplied template file.  Section 4 describes
file-handling functions which control output redirection,
and section 5 describes
user-accessible global variables and mode switches which alter the code
generation process.  Finally, section 6 presents three complete examples.
.LP
Throughout this manual, examples are given from both the user
and the LISP levels of VAXIMA.  It is assumed that the user knows
that a <control>D typed at the user level puts VAXIMA into LISP
mode and vice versa, and that programming at the LISP level is in
Franz LISP.
.NH 3
Typographic Conventions
.LP
The following conventions are used in the syntactic definitions of
functions throughout this manual:
.IP -
Command parts which must be typed exactly as shown are \fIunderlined\fR.
.IP -
User-supplied arguments are not underlined.
.IP -
{ ... } indicate optional command parts and are not actually part of
the function call syntax.
.LP
The syntax of each GENTRAN function is shown terminated with a
\fB;\fR.  However, either \fB;\fR or \fB$\fR can be used to terminate any
command with the usual VAXIMA meaning:  \fB;\fR indicates that the
returned value is to be printed, while \fB$\fR indicates that printing
of the returned value is to be suppressed.
