.NH 1
OUTPUT REDIRECTION
.LP
Most examples given thus far in this manual have sent all
generated code to the terminal screen.  In actual code generation
applications, however, code must be sent to a file
which will be compiled at a later time.  This section
explains methods of redirecting code to a file as it is generated.  Any
number of output files can be open simultaneously, and generated code
can be sent to any combination of these open files.
.NH 2
File Selection Commands
.LP
VAXIMA provides the user with two file handling commands for
output redirection:  \fIwritefile\fR and \fIclosefile\fR.  The
\fIwritefile\fR command takes a single file name as argument and
directs all VAXIMA output to that file from then on, until another
\fIwritefile\fR changes the output file, or \fIclosefile\fR explicitly
closes it.  Output can go to only one file at a time, and only one
file can be open at any given time.  Any existing file is erased before
its first use for output in a job.
.LP
If an output file is closed with a \fIclosefile\fR command, and a
further \fIwritefile\fR command is issued for that same file, the
contents of the file are erased before the new output is written.  When
an output file is closed with the \fIclosefile\fR command, output
is switched to the terminal.
.LP
These commands are suitable for most applications in which VAXIMA
output must be saved.  However, they have two deficiencies
when considered for use in code generation applications.  First, they
are inconvenient.  \fIwritefile\fR tells VAXIMA to direct \fIall\fR
output to the specified file.  Thus, in addition to output
written as side effects of function calls, returned values are
also written to the file.  Secondly, the \fIwritefile\fR command does
not allow output to be sent to two or more files without reissuing the
\fIwritefile\fR with another file name.  In an effort to remove these
deficiencies and make the code generation commands flexible and
easy to use, separate file handling commands are provided by
GENTRAN which redirect generated code \fIonly\fR.
.LP
The \fIgentranout\fR and \fIgentranshut\fR commands are identical to
the VAXIMA \fIwritefile\fR and \fIclosefile\fR commands with the
following exceptions:
.IP -
\fIgentranout\fR and \fIgentranshut\fR redirect \fIonly\fR code which
is printed as a side effect of GENTRAN commands.
.IP -
\fIgentranout\fR allows more than one file name to be given
to indicate that generated code is to be sent to two or
more files.  (It is particularly convenient to be able
to have generated code sent to the terminal screen and one or
more files simultaneously.)
.IP -
\fIgentranout\fR does not automatically erase existing files;
instead, it appends output onto the end of files.
.LP
The next two subsections describe these commands in detail.
.NH 3
gentranout
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIgentranout(\fRf1,f2,...,fn);
.ft
.DE
.RE
where f1,f2,...,fn is a list of one or more f's, and
each f is one of:
.RS
.DS L
.ft CR
a string  =  an output file
\fItrue\fR      =  the terminal
\fIfalse\fR     =  the current output file(s)
\fIall\fR       =  all files currently open for output by GENTRAN
.ft
.DE
.RE
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(gentranout '(\fRf1 f2 ... fn))
.ft
.DE
.RE
where f1 f2 ... fn is a sequence of one or more f's, and
each f is one of:
.RS
.DS L
.ft CR
an atom  =  an output file
\fIt\fR        =  the terminal
\fInil\fR      =  the current output file(s)
\fI$all\fR     =  all files currently open for output by GENTRAN
.ft
.DE
.RE
.SH
Side Effects:
.LP
GENTRAN maintains a list of files currently open for
output by GENTRAN commands \fIonly\fR.  \fIgentranout\fR inserts each
file name represented by f1,f2,...,fn into that list and
opens each one for output.  It also resets the current output
file(s) to be all files in f1,f2,...,fn.
.SH
Return Value:
.LP
\fIgentranout\fR returns the list of files represented by
f1,f2,...,fn; i.e., the current output file(s) after the command
has been executed.
.SH
Diagnostic Messages:
.LP
Wrong Type of Arg
.SH
Examples:
.DS L
.ft CR
Output file list:

  current-output
  |
  v
+------+
| t    |
+------+
.ft
.DE
.DS L
.ft CR
(c1) gentranout("f1");

(d1)                       f1
.ft
.DE
.DS L
.ft CR
Output file list:

         current-output
         |
         v
+------+------+
| t    | "f1" |
+------+------+
.ft
.DE
.DS L
.ft CR
(c2) gentranout("f2");

(d2)                       f2
.ft
.DE
.DS L
.ft CR
Output file list:

                current-output
                |
                v
+------+------+------+
| t    | "f1" | "f2" |
+------+------+------+
.ft
.DE
.DS L
.ft CR
(c3) gentranout(true,"f3");

(d3)                       [true,f3]
.ft
.DE
.DS L
.ft CR
Output file list:

  +--------------------+------current-output
  |                    |
  v                    v
+------+------+------+------+
| t    | "f1" | "f2" | "f3" |
+------+------+------+------+
.ft
.DE
.DS L
.ft CR
(c4) gentranout("f1");

(d4)                       f1
.ft
.DE
.DS L
.ft CR
Output file list:

         current-output
         |
         v
+------+------+------+------+
| t    | "f1" | "f2" | "f3" |
+------+------+------+------+
.ft
.DE
.NH 3
gentranshut
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIgentranshut(\fRf1,f2,...,fn);
.ft
.DE
.RE
where f1,f2,...,fn is a list of one or more f's, and each
f is one of:
.RS
.DS L
.ft CR
a string  =  an output file
\fIfalse\fR     =  the current output file(s)
\fIall\fR       =  all files currently open for output by GENTRAN
.ft
.DE
.RE
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(gentranshut '(\fRf1 f2 ... fn))
.ft
.DE
.RE
where f1 f2 ... fn is a list of one or more f's, and
each f is one of:
.RS
.DS L
.ft CR
an atom  =  an output file
\fInil\fR      =  the current output file(s)
\fI$all\fR     =  all files currently open for output by GENTRAN
.ft
.DE
.RE
.SH
Side Effects:
.LP
\fIgentranshut\fR creates a list of file names from
f1,f2,...,fn, deletes each from the output file list,
and closes the corresponding files.  If (all of) the current
output file(s) are closed, then the current output file is reset to the
terminal.
.SH
Return Value:
.LP
\fIgentranshut\fR returns (a list of) the current output file(s) after
the command has been executed.
.SH
Diagnostic Messages:
.LP
File Not Open for Output
.LP
Wrong Type of Arg
.SH
User Level Examples:
.DS L
.ft CR
Output file list:

  current-output ------+------+--------------------+
                       |      |                    |
                       v      v                    v
+------+------+------+------+------+------+------+------+
| t    | "f1" | "f2" | "f3" | "f4" | "f5" | "f6" | "f7" |
+------+------+------+------+------+------+------+------+
.ft
.DE
.DS L
.ft CR
(c1) gentranshut("f1","f2","f7");

(d1)                       [f3,f4]
.ft
.DE
.DS L
.ft CR
Output file list:

         +------+----------- current-output
         |      |
         v      v
+------+------+------+------+------+
| t    | "f3" | "f4" | "f5" | "f6" |
+------+------+------+------+------+
.ft
.DE
.DS L
.ft CR
(c2) gentranshut(false);

(d2)                       true
.ft
.DE
.DS L
.ft CR
Output file list:

  current-output
  |
  v
+------+------+------+
| t    | "f5" | "f6" |
+------+------+------+
.ft
.DE
.DS L
.ft CR
(c3) gentranshut(all);

(d3)                       true
.ft
.DE
.DS L
.ft CR
Output file list:

  current-output
  |
  v
+------+
| t    |
+------+
.ft
.DE
.NH 2
Operations on the Output File Stack
.LP
Section 4.1 explained the \fIgentranout\fR and \fIgentranshut\fR
commands which are very similar to the VAXIMA \fIwritefile\fR and
\fIclosefile\fR commands, but redirect only code generated as side
effects of GENTRAN commands to files.  This section describes another
pair of file handling commands provided by GENTRAN.
.LP
In some code generation applications it may be convenient to
be able to send generated code to one (set of) file(s), then
temporarily send code to another (set of) file(s), and later
resume sending generated code to the first (set of) file(s).  In
other words, it is convenient to think of the output
files as being arranged in a stack which can be pushed
whenever new files are to be written to temporarily, and popped
whenever previously written-to files are to be appended
onto.  \fIgentranpush\fR and \fIgentranpop\fR enable the user to
manipulate a stack of open output files in these ways.
.LP
\fIgentranpush\fR simply pushes a (set of) file(s) onto the stack
and opens each one that is not already open for output.  \fIgentranpop\fR
deletes the top-most occurrence of the given
file(s) from the stack and closes each one that is no longer
in the stack.  The stack is initialized to one element:  the
terminal.  This element is always on the bottom of the
stack, and thus, is the default output file.  The current output
file is always the file(s) on top of the stack.
.NH 3
gentranpush
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIgentranpush(\fRf1,f2,...,fn);
.ft
.DE
.RE
where f1,f2,...,fn is a list of one or more f's, and each
f is one of:
.RS
.DS L
.ft CR
a string  =  an output file
\fItrue\fR      =  the terminal
\fIfalse\fR     =  the current output file(s)
\fIall\fR       =  all files currently open for output by GENTRAN
.ft
.DE
.RE
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(gentranpush '(\fRf1 f2 ... fn))
.ft
.DE
.RE
where f1 f2 ... fn is a list of one or more f's, and
each f is one of:
.RS
.DS L
.ft CR
an atom  =  an output file
\fIt\fR        =  the terminal
\fInil\fR      =  the current output file(s)
\fI$all\fR     =  all files currently open for output by GENTRAN
.ft
.DE
.RE
.SH
Side Effects:
.LP
\fIgentranpush\fR creates a list of file name(s) represented by
f1,f2,...,fn and pushes that list onto the output
stack.  Each file in the list that is not already open
for output is opened at this time.  The current output
file is reset to this new element on the top of the
stack.
.SH
Return Value:
.LP
\fIgentranpush\fR returns the list of files represented by
f1,f2,...,fn; i.e., the current output file(s) after
the command has been executed.
.SH
Diagnostic Messages:
.LP
Wrong Type of Arg
.SH
User Level Examples:
.DS L
.ft CR
Output stack:

+-------------+
| t           |  <-- current-output
+-------------+
.ft
.DE
.DS L
.ft CR
(c1) gentranpush("f1");

(d1)                       f1
.ft
.DE
.DS L
.ft CR
Output stack:

+-------------+
| "f1"        |  <-- current-output
| t           |
+-------------+
.ft
.DE
.DS L
.ft CR
(c2) gentranpush("f2","f3");

(d2)                       [f2,f3]
.ft
.DE
.DS L
.ft CR
Output stack:

+-------------+
| "f2" "f3"   |  <-- current-output
| "f1"        |
| t           |
+-------------+
.ft
.DE
.DS L
.ft CR
(c3) gentranpush(false,true);

(d3)                       [f2,f3,true]
.ft
.DE
.DS L
.ft CR
Output stack:

+-------------+
| "f2" "f3" t |  <-- current-output
| "f2" "f3"   |
| "f1"        |
| t           |
+-------------+
.ft
.DE
.DS L
.ft CR
(c4) gentranpush("f1");

(d4)                       f1
.ft
.DE
.DS L
.ft CR
Output stack:

+-------------+
| "f1"        |  <-- current-output
| "f2" "f3" t |
| "f2" "f3"   |
| "f1"        |
| t           |
+-------------+
.ft
.DE
.NH 3
gentranpop
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIgentranpop(\fRf1,f2,...,fn);
.ft
.DE
.RE
where f1,f2,...,fn is a list of one or more f's, and each
f is one of:
.RS
.DS L
.ft CR
a string  =  an output file
\fItrue\fR      =  the terminal
\fIfalse\fR     =  the current output file(s)
\fIall\fR       =  all files currently open for output by GENTRAN
.ft
.DE
.RE
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(gentranpop '(\fRf1 f2 ... fn))
.ft
.DE
.RE
where f1 f2 ... fn is a list of one or more f's, and
each f is one of:
.RS
.DS L
.ft CR
an atom  =  an output file
\fIf\fR        =  the terminal
\fInil\fR      =  the current output file(s)
\fI$all\fR     =  all files currently open for output by GENTRAN
.ft
.DE
.RE
.SH
Side Effects:
.LP
\fIgentranpop\fR deletes the top-most occurrence of the single
element containing the file name(s) represented by
f1,f2,...,fn from the output stack.  Files whose names
have been completely removed from the output stack are
closed.  The current output file is reset to the (new)
element on the top of the output stack.
.SH
Return Value:
.LP
\fIgentranpop\fR returns the current output file(s) after
this command has been executed.
.SH
Diagnostic Messages
.LP
File Not Open for Output
.LP
Wrong Type of Arg
.SH
User Level Examples:
.DS L
.ft CR
Output stack:

+-------------+
| "f4"        |  <-- current-output
| "f4" "f2" t |
| "f4"        |
| t           |
| "f3"        |
| "f2" "f1"   |
| t           |
+-------------+
.ft
.DE
.DS L
.ft CR
(c1) gentranpop(false);

(d1)                       [f4,f2,true]
.ft
.DE
.DS L
.ft CR
Output stack:

+-------------+
| "f4" "f2" t |  <-- current-output
| "f4"        |
| t           |
| "f3"        |
| "f2" "f1"   |
| t           |
+-------------+
.ft
.DE
.DS L
.ft CR
(c2) gentranpop("f2","f1");

(d2)                       [f4,f2,true]
.ft
.DE
.DS L
.ft CR
Output stack:

+-------------+
| "f4" "f2" t |  <-- current-output
| "f4"        |
| t           |
| "f3"        |
| t           |
+-------------+
.ft
.DE
.DS L
.ft CR
(c3) gentranpop(false);

(d3)                       f4
.ft
.DE
.DS L
.ft CR
Output stack:

+-------------+
| "f4"        |  <-- current-output
| t           |
| "f3"        |
| t           |
+-------------+
.ft
.DE
.DS L
.ft CR
(c4) gentranpop(all);

(d4)                       true
.ft
.DE
.DS L
.ft CR
Output stack:

+-------------+
| t           |  <-- current-output
+-------------+
.ft
.DE
.NH 2
Temporary Output Redirection
.LP
Sections 2.2 and 3.1 explain how to use the code generation
and template processing commands.  The syntax for these two
commands is:
.RS
.DS L
.ft CR
\fIgentran(\fRstmt1,stmt2,...,stmtn {,[f1,f2,...,fn]});
.ft
.DE
.RS
and
.RE
.DS L
.ft CR
\fIgentranin(\fRf1,f2,...,fn {,[f1,f2,...,fm]});
.ft
.DE
.RE
The optional parts of these two commands can be used for \fItemporary\fR
output redirection; they can be used when the current output file
is to be temporarily reset, for this command only.
.LP
Thus the following two sequences of commands are equivalent:
.RS
.DS  L
.ft CR
(c1) \fIgentranpush\fR(f1,f2,...,fm)$

(c2) \fIgentran\fR(stmt1,stmt2,...,stmtn)$

(c3) \fIgentranpop\fR(false)$
.ft
.DE
.RS
and
.RE
.DS L
.ft CR
(c1) \fIgentran(\fRstmt1,stmt2,...,stmtn, [f1,f2,...,fm])$
.ft
.DE
.RE
