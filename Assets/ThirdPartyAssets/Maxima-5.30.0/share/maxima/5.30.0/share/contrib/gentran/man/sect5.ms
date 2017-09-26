.NH 1
MODIFICATION OF THE CODE GENERATION PROCESS
.LP
GENTRAN is designed to be flexible enough to be used in a variety of
code generation applications.  For this reason, several mode
switches and variables are provided to enable the user to tailor the
code generation process to meet his or her particular needs.
.NH 2
Mode Switches
.LP
Two special functions, \fIon\fR and \fIoff\fR, are provided to enable
the user to turn GENTRAN mode switches on and off:
.RS
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIon(\fRflag1,flag2,...,flagn);
.ft
.DE
.DS L
.ft CR
\fIoff(\fRflag1,flag2,...,flagn);
.ft
.DE
.RE
where flag1,flag2,...,flagn is a list of one or more
flag's, and each flag is one of the mode switches
described in this section.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(on '(\fRflag1 flag2 ... flagn))
.ft
.DE
.DS L
.ft CR
\fI(off \fR'(flag1 flag2 ... flagn))
.ft
.DE
.RE
where flag1 flag2 ... flagn is a list of one or more flag's,
and each flag is one of the mode switches described in this
section.
.SH
Side Effects:
.LP
When the \fIon\fR function is called, each of the LISP variables
corresponding to flag1,flag2,...,flagn is set to \fIt\fR.  Similarly,
when the \fIoff\fR function is called, each of the corresponding LISP
variables is set to \fInil\fR.
.LP
When either function is called, the property lists of each of the
corresponding LISP variables are also checked.  Each property list
that contains a \fIsimpfg\fR property and has a function call as
part of the property's value also has that function call evaluated.
.SH
Return Value:
.LP
Each of these functions returns the atom \fIdone\fR (\fI$done\fR at
the LISP level).
.RE
.LP
The following GENTRAN mode switches can be turned on and off with the
\fIon\fR and \fIoff\fR functions:
.RS
.SH
fortran
.LP
- when turned on, causes all VAXIMA statements and expressions typed in
at the user's terminal
to be translated into FORTRAN code until an \fIoff(fortran)$\fR
command is issued
.br
The special GENTRAN functions \fIeval\fR,
\fIlsetq\fR, \fIrsetq\fR, \fIlrsetq\fR, \fItype\fR, and \fIliteral\fR,
can be entered at the user level of VAXIMA while \fIfortran\fR is on.
.br
All generated code is written to the current output file(s).
.br
See also section 2.6.
.br
- default setting:  off
.SH
ratfor
.LP
- when turned on, causes all VAXIMA statements and expressions typed in
at the user's terminal
to be translated into RATFOR code until an \fIoff(ratfor)$\fR
command is issued
.br
The special GENTRAN functions \fIeval\fR,
\fIlsetq\fR, \fIrsetq\fR, \fIlrsetq\fR, \fItype\fR, and \fIliteral\fR,
can be entered at the user
level of VAXIMA while \fIratfor\fR is on.
.br
All generated code is written to the current output file(s).
.br
See also section 2.6.
.br
- default setting:  off
.SH
c
.LP
- when turned on, causes all VAXIMA statements and expressions typed in
at the user's terminal
to be translated into C code until an \fIoff(c)$\fR command is
issued
.br
The special GENTRAN functions \fIeval\fR,
\fIlsetq\fR, \fIrsetq\fR, \fIlrsetq\fR, \fItype\fR, and \fIliteral\fR,
can be entered at the user level of VAXIMA while \fIc\fR is on.
.br
All generated code is written to the current output file(s).
.br
See also section 2.6.
.br
- default setting:  off
.SH
gendecs
.LP
- When turned on, enables type declarations to be generated automatically.
.br
As long as \fIgendecs\fR is on, declaration information is extracted from
the Symbol Table and converted into declarations as soon as possible.
.br
However, when \fIgendecs\fR is turned off, declarations are not generated;
information is only stored in the Symbol Table.
.br
If \fIgendecs\fR is turned back on after being off, all type
information is extracted from the Symbol Table, and declarations are
generated immediately (i.e., \fIgendecs(false);\fR is evaluated).
.br
See also sections 2.8.2 and 3.2.
.br
- default setting:  on
.SH
float
.LP
- when turned on, causes integers in generated numerical code to be
converted to floating point numbers except in the following places:  array
subscripts, powers to which expressions are raised (in exponentiation), and
initial, final, and step values in do loops.
.br
- default setting:  on
.SH
gentranseg
.LP
- when turned on, checks the print length of expressions and breaks
those expressions that are longer than \fI?maxexpprintlen\e*\fR
(\fImaxexpprintlen*\fR) down
into subexpressions and assigns them to temporary variables
.br
See also section 2.8.
.br
- default setting:  on
.SH
gentranopt
.LP
- when turned on, replaces each block of straightline code by
an optimized sequence of assignments
.br
The \fIoptimize\fR facility takes an assignment and replaces common
subexpressions by temporary variables.  It returns the resulting assignment
statements
with common-subexpression-to-temporary-variable assignment
statements preceding them
.br
- default setting:  off
.SH
gentranparser
.LP
- when turned off, improves translation speed by disabling the GENTRAN
parser module.
.br
Note:  when this flag is off, error messages for statements which cannot
be translated will not be printed.
.br
- default setting:  off
.RE
.NH 2
Variables
.LP
Several global variables are provided in GENTRAN to enable the
user to
.IP -
select the target language
.IP -
control expression optimization and segmentation
.IP -
change automatically generated variable names and statement numbers
.IP -
modify the code formatter
.LP
The following four subsections describe these variables.  Each variable
is given in its user level syntax, followed by its LISP level syntax.
.NH 3
Target Language Selection
.RS
.SH
?gentranlang\e*         gentranlang*
.LP
- target language (FORTRAN, RATFOR, or C)
.br
See also section 2.1.
.br
- value type:  atom
.br
- default value:  fortran
.SH
?gentranopt\e*          gentranopt*           \fR**  kentvax only  **
.LP
- system under which code is to be optimized (VAXIMA or REDUCE)
.br
See also section 2.7.
.br
- value type:  atom
.br
- default value:  vaxima
.RE
.NH 3
Expression Optimization & Segmentation Control
.RS
.SH
?maxexpprintlen\e*      maxexpprintlen*
.LP
- value used to determine whether or not an expression should be
segmented; maximum number of characters permitted in an expression
in the target language (excluding spaces printed for formatting)
.br
See also section 2.8.
.br
- value type:  integer
.br
- default value:  800
.RE
.NH 3
Temporary Variable Names & Statement Numbers
.RS
.SH
?tempvarname\e*         tempvarname*
.LP
- name used as prefix in generating temporary variable names
.br
See also section 2.9.
.br
- value type:  atom
.br
- default value:  t
.SH
?tempvarnum\e*          tempvarnum*
.LP
- number appended to \fI?tempvarname\e*\fR to create a temporary variable
name
.br
If the temporary variable name resulting from appending
\fI?tempvarnum\e*\fR onto \fI?tempvarname\e*\fR has already been generated
and still holds a useful value, then \fI?tempvarnum\e*\fR is incremented
and temporary variable names are compressed until one is found which
was not previously generated or does not still hold a significant value.
.br
See also section 2.9.
.br
- value type:  integer
.br
- default value:  0
.SH
?tempvartype\e*         tempvartype*
.LP
- target language variable type (e.g., integer, real*8, float, etc) used
as a default for automatically generated variables whose type cannot be
determined otherwise
.br
If \fI?tempvartype\e*\fR is false (nil), then generated temporary
variables whose type cannot be determined are not automatically declared.
.br
See also section 2.8.
.br
- value type:  atom
.br
- default value:  false (nil)
.SH
?genstmtno\e*           genstmtno*
.LP
- number used when a statement number must be generated
.br
- value type:  integer
.br
- default value:  25000
.SH
?genstmtincr\e*         genstmtincr*
.LP
- number by which \fI?genstmtno\e*\fR is increased each time a new
statement number is generated
.br
- value type:  integer
.br
- default value:  1
.RE
.NH 3
Code Formatting
.RS
.SH
?fortcurrind\e*         fortcurrind*
.LP
- number of blank spaces printed at the beginning of each line of
generated FORTRAN code
.br
- value type:  integer
.br
- default value:  6
.SH
?ratcurrind\e*          ratcurrind*
.LP
- number of blank spaces printed at the beginning of each line of
generated RATFOR code
.br
- value type:  integer
.br
- default value:  0
.SH
?ccurrind\e*            ccurrind*
.LP
- number of blank spaces printed at the beginning of each line of
generated C code
.br
- value type:  integer
.br
- default value:  0
.SH
?tablen\e*              tablen*
.LP
- number of blank spaces printed for each new level of indentation
.br
Automatic indentation can be turned off by setting this variable to 0.
.br
- value type:  integer
.br
- default value:  4
.SH
?fortlinelen\e*         fortlinelen*
.LP
- maximum number of characters printed on each line of generated FORTRAN code
.br
- value type:  integer
.br
- default value:  72
.SH
?ratlinelen\e*          ratlinelen*
.LP
- maximum number of characters printed on each line of generated RATFOR code
.br
- value type:  integer
.br
- default value:  80
.SH
?clinelen\e*            clinelen*
.LP
- maximum number of characters printed on each line of generated C code
.br
- value type:  integer
.br
- default value:  80
.RE
