.NH 1
CODE GENERATION
.LP
GENTRAN generates numerical programs based on algorithmic specifications
in the VAXIMA programming language and derived symbolic expressions
produced by VAXIMA evaluations.  FORTRAN, RATFOR, or C code can
be produced.  Type declarations can be generated, and comments and
other literal strings can be inserted into the generated code.  In
addition, large arithmetic expressions can be optimized and segmented
into a sequence of subexpressions of manageable size.
.LP
This section explains how to select the target language, generate
code, control expression optimization and segmentation, and how to
generate temporary variable names.
.NH 2
Target Language Selection
.LP
Before generating code, the target numerical language must be
selected.  GENTRAN is currently able to generate FORTRAN, RATFOR,
and C code.  The global lisp variable \fI*gentranlang\fR determines
which type of code is produced.  \fI*gentranlang\fR can be
set at the user and LISP levels.
It can be set to any value, but
only three atoms have special meaning:  \fIfortran\fR, \fIratfor\fR, and
\fIc\fR.  Any other value is assumed to mean
\fIfortran\fR.  \fI*gentranlang\fR is always initialized to \fIfortran\fR.
At the Macsyma user level,
.DS
.ft CR
gentranlang(ratfor);
.ft
.DE
for example, sets \fI*gentranlang\fR to \fIratfor\fR.
.NH 2
Translation
.LP
The \fIgentran\fR (\fIgen\fRerate/\fItran\fRslate) command is used to generate
numerical code and also to translate code from algorithmic
specifications in the VAXIMA programming language to code in
the target numerical language.  Section 2.3 explains code
\fIgeneration\fR.  This section explains code \fItranslation\fR.
.LP
A substantial subset of all expressions and statements in the
VAXIMA programming language can be translated directly into
numerical code.  The \fIgentran\fR command takes VAXIMA
statements or procedure definitions, and translates them
into code in the target language.
.SH
User-Level Syntax:
.RS
.DS L
.ft CR
\fIgentran(\fRstmt1,stmt2,...,stmtn {,[f1,f2,...,fm]});
.ft
.DE
.RE
where stmt1,stmt2,...,stmtn is a sequence of
one or more stmt's, each of which is any VAXIMA user level
expression, (simple, group, or block) statement, or procedure definition
that can be translated by GENTRAN into the target language[1].  Each
.FS
[1] See Appendix A for a complete list of VAXIMA user-level expression
and statement types that can be translated.
.FE
stmt may contain calls to the special functions described in
sections 2.2.1 - 2.2.5 and 2.3.1 - 2.3.4.
.LP
[f1,f2,...,fm] is an optional argument list containing one or
more f's, where each f is one of:
.RS
.DS L
.ft CR
a string  =  an output file
\fItrue\fR      =  the terminal
\fIfalse\fR     =  the current output file(s)
\fIall\fR       =  all files currently open for output by GENTRAN
             (see sect. 4)
.ft
.DE
.RE
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(gentran '(\fRstmt1 stmt2 ... stmtn) '(f1 f2 ... fm))
.ft
.DE
.RE
where stmt1 stmt2 ... stmtn is a sequence of one or more
stmt's, each of which is any VAXIMA LISP level expression,
(simple, group, or block) statement, or procedure definition that
can be translated by GENTRAN into the target language[2].  Each
.FS
[2] See Appendix A for a complete list of VAXIMA LISP level
expression and statement types that can be translated.
.FE
stmt may contain calls to the special functions described in
sections 2.2.1 - 2.2.5 and 2.3.1 - 2.3.4.
.LP
(f1 f2 ... fm) is an argument list containing any number
of f's, where each f is one of:
.RS
.DS L
.ft CR
a string  =  an output file
\fIt\fR         =  the terminal
\fInil\fR       =  the current output file(s)
\fI$all\fR      =  all files currently open for output by GENTRAN
             (see sect. 4)
.ft
.DE
.RE
.SH
Side Effects:
.LP
\fIgentran\fR translates each stmt into formatted code in the
target language.
.LP
If the file list is empty, generated
code is simply written to the current output file.  However,
if it contains one or more file arguments,
then the current output file is temporarily
overridden.  Generated code is written to each file represented
by f1,f2,...,fm for this command only.  Files which were open
prior to the call to \fIgentran\fR will remain open after the call,
and files which did not exist or were not open prior
to the call will be created (if necessary), opened, written
to (or appended onto), and closed.  The output file stack
will be exactly the same both before and after the call.
.SH
Return Value:
.LP
\fIgentran\fR returns (a list of) the name(s) of the file(s) to
which code was written.
.SH
Diagnostic Messages:
.LP
wrong type of arg
.LP
exp
.br
cannot be translated
.SH
User Level Examples:
.DS L
.ft CR
(c1) gentranlang(fortran)$
.ft
.DE
.DS L
.ft CR
(c2) gentran( for i:1 thru n do v[i]:0.0 );
      do 25001 i=1,n
          v(i)=0.0
25001 continue

(d2)                       true
.ft
.DE
.DS L
.ft CR
(c3) gentranlang(ratfor)$
.ft
.DE
.DS L
.ft CR
(c4) gentran( for i:1 thru n do
                     for j:i+1 thru n do
                     (
                         x[j,i] : x[i,j],
                         y[j,i] : y[i,j]
                     )                    );
do i=1,n
    do j=i+1,n
        {
            x(j,i)=x(i,j)
            y(j,i)=y(i,j)
        }

(d4)                       true
.ft
.DE
.DS L
.ft CR
(c5) gentranlang(c)$
.ft
.DE
.DS L
.ft CR
(c6) gentran( p : a*x^2 + b*x + c );
p=a*power(x,2)+b*x+c;

(d6)                       true
.ft
.DE
.SH
LISP Level Examples:
.DS L
.ft CR
<1>: (setq *gentranlang 'fortran)
fortran
.ft
.DE
.DS L
.ft CR
<1>: (gentran '(((mdo) i 1 nil nil n nil ((msetq) ((v) i) 0.0))) ())
      do 25001 i=1,n
          v(i)=0.0
25001 continue
t
.ft
.DE
.DS L
.ft CR
<1>: (setq *gentranlang 'ratfor)
ratfor
.ft
.DE
.DS L
.ft CR
<1>: (gentran '(((mdo) i 1 nil nil n nil
                       ((mdo) j ((mplus) i 1) nil nil n nil
                              ((mprogn)
                               ((msetq) ((x) j i) ((x) i j))
                               ((msetq) ((y) j i) ((y) i j)))))) ())
do i=1,n
    do j=i+1,n
        {
            x(j,i)=x(i,j)
            y(j,i)=y(i,j)
        }
t
.ft
.DE
.DS L
.ft CR
<1>: (setq *gentranlang 'c)
c
.ft
.DE
.DS L
.ft CR
<1>: (gentran '(((msetq) p ((mplus) ((mtimes) a ((mexpt) x 2))
                                    ((mtimes) b x)
                                    c))) ())
p=a*power(x,2)+b*x+c;
t
.ft
.DE
.NH 3
Special Arguments
.LP
In addition to statements in the VAXIMA programming language, the
\fIgentran\fR function can also translate special arguments
into numerical code.  Several of these arguments are described
in this section.
.SH
Subprogram Headings & Bodies
.LP
FORTRAN and RATFOR function and subroutine headings, and C procedure
headings can be generated separately from their corresponding bodies
of code with the arguments given in this section.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIsubroutine(\fRname(p1,p2,...,pn))
.sp
\fIfunction({\fRtype,} name(p1,p2,...,pn))
.sp
\fIcprocedure(\fR{type,} name(p1,p2,...,pn))
.sp
\fIbody(\fRstmt1,stmt2,...,stmtn)
.ft
.DE
.RE
where name is an atom, and p1,p2,...,pn is a sequence
of zero or more atoms, representing a subroutine, function or
procedure name, and a list of parameter names, respectively; type
is an optional argument which, if supplied to either
\fIfunction\fR or \fIcprocedure\fR, is a string representing the
function's or procedure's return value type (e.g., "real*8",
"double", etc.); and stmt1,stmt2,...,stmtn is a sequence of zero or
more user level translatable VAXIMA statements representing the
body of a subprogram.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(($subroutine) \fR((name) p1 p2 ... pn))
.sp
\fI(($function) \fR{type} ((name) p1 p2 ... pn))
.sp
\fI(($cprocedure) \fR{type} ((name) p1 p2 ... pn))
.sp
\fI(($body) \fRstmt1 stmt2 ... stmtn)
.ft
.DE
.RE
where name is an atom, and p1 p2 ... pn is a sequence
of zero or more atoms, representing a subroutine function or
procedure name, and a list of parameter names, respectively;
type is an optional argument which, if supplied to either
\fI$function\fR or \fI$cprocedure\fR, is an atom representing the
function's or procedure's return value type (e.g., real*8, double, etc.);
and stmt1 stmt2 ... stmtn is a sequence of zero or more LISP
level translatable VAXIMA statements representing the body of a
subprogram.
.SH
User Level Examples:
.DS L
.ft CR
(c1) gentranlang(fortran)$
.ft
.DE
.DS L
.ft CR
(c2) gentran( function("real", f(x,y)),
              body( x:x^2, y:y^2, f:abs(x-y) ) )$
      real function f(x,y)
      x=x**2
      y=y**2
      f=abs(x-y)
      return
      end
.ft
.DE
.DS L
.ft CR
(c3) gentranlang(ratfor)$
.ft
.DE
.DS L
.ft CR
(c4) gentran( subroutine(f(x,y)),
              body( y:abs(x^2-y^2) ) )$
subroutine f(x,y)
y=abs(x**2-y**2)
return
end
.ft
.DE
.DS L
.ft CR
(c5) gentranlang(c)$
.ft
.DE
.DS L
.ft CR
(c6) gentran( cprocedure("float", f(x,y)) )$
float f(x,y)
.ft
.DE
.SH
LISP Level Examples:
.DS L
.ft CR
<1>: (setq *gentranlang 'fortran)
fortran
.ft
.DE
.DS L
.ft CR
<1>: (gentran '( (($function) real ((f) x y))
                 (($body) ((msetq) x ((mexpt) x 2))
                          ((msetq) y ((mexpt) y 2))
                          ((msetq) f ((mabs)
                                      ((mplus) x ((mminus) y))))))
              ())
      real function f(x,y)
      x=x**2
      y=y**2
      f=abs(x-y)
      return
      end
t
.ft
.DE
.DS L
.ft CR
<1>: (setq *gentranlang 'ratfor)
ratfor
.ft
.DE
.DS L
.ft CR
<1>: (gentran '( (($subroutine) ((f) x y))
                 (($body) ((msetq) y
                                   ((mabs)
                                    ((mplus) ((mexpt) x 2)
                                             ((mminus) ((mexpt) y 2)))))))
              ())
subroutine f(x,y)
y=abs(x**2-y**2)
return
end
t
.ft
.DE
.DS L
.ft CR
<1>: (setq *gentranlang 'c)
c
.ft
.DE
.DS L
.ft CR
<1>: (gentran '( (($cprocedure) float ((f) x y)) ) ())
float f(x,y)
t
.ft
.DE
.SH
Other Special Arguments
.LP
The arguments described in this section allow numerical language statements
to be generated which do not have semantically equivalent statements in
the VAXIMA programming language.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIbreak()\fR
.ft
.DE
.DS L
.ft CR
\fIstop()\fR
.ft
.DE
.DS L
.ft CR
\fIend()\fR
.ft
.DE
.DS L
.ft CR
\fIbegin_group\fR
.ft
.DE
.DS L
.ft CR
\fIend_group\fR
.ft
.DE
.RE
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(($break))\fR
.ft
.DE
.DS L
.ft CR
\fI(($stop))\fR
.ft
.DE
.DS L
.ft CR
\fI(($end))\fR
.ft
.DE
.DS L
.ft CR
\fI$begin_group\fR
.ft
.DE
.DS L
.ft CR
\fI$end_group\fR
.ft
.DE
.RE
.SH
User Level Examples:
.DS L
.ft CR
(c1) gentranlang(fortran)$
.ft
.DE
.DS L
.ft CR
(c2) gentran( for n:1 thru 100 do
                  if f(n)<0 then break() )$
      do 25001 n=1,100
          if (.not.f(n).lt.0.0) goto 25002
              goto 25003
25002     continue
25001 continue
25003 continue
.ft
.DE
.DS L
.ft CR
(c3) gentran( stop(), end() )$
      stop
      end
.ft
.DE
.DS L
.ft CR
(c4) gentranlang(ratfor)$
.ft
.DE
.DS L
.ft CR
(c5) gentran( for n:1 thru 100 do
                  if f(n)<0 then break() )$
do n=1,100
    if (f(n)<0.0)
        break
.ft
.DE
.DS L
.ft CR
(c6) gentranlang(c)$
.ft
.DE
.DS L
.ft CR
(c7) gentran( begin_group,
              for n:1 thru 100 do
                  if f(n)<0 then break() )$
{
    for (n=1;!(n>100);n=n+1)
        fi(f(n)<0.0)
            break;
.ft
.DE
.DS L
.ft CR
(c8) gentran( if n>100 then stop(),
              end_group )$
      if (n>100.0)
          exit(0);
}
.ft
.DE
.SH
LISP Level Examples:
.DS L
.ft CR
<1>: (setq *gentranlang 'fortran)
fortran
.ft
.DE
.DS L
.ft CR
<1>: (gentran '( ((mdo) n 1 nil nil 100 nil
                        ((mcond) ((mlessp) ((f) n) 0)
                                 (($break))
                                 t
                                 $false)) ) ())
      do 25001 n=1,100
          if (.not.f(n).lt.0.0) goto 25002
              goto 25003
25002     continue
25001 continue
25003 continue
t
.ft
.DE
.DS L
.ft CR
<1>: (gentran '( (($stop)) (($end)) ) ())
      stop
      end
t
.ft
.DE
.DS L
.ft CR
<1>: (setq *gentranlang 'ratfor)
ratfor
.ft
.DE
.DS L
.ft CR
<1>: (gentran '( ((mdo) n 1 nil nil 100 nil
                        ((mcond) ((mlessp) ((f) n) 0)
                                 (($break))
                                 t
                                 (($false)))) ) ())
do n=1,100
    if (f(n)<0.0)
        break
t
.ft
.DE
.DS L
.ft CR
<1>: (setq *gentranlang 'c)
c
.ft
.DE
.DS L
.ft CR
<1>: (gentran '( $begin_group
                 ((mdo) n 1 nil nil 100 nil
                        ((mcond) ((mlessp) ((f) n) 0)
                                 (($break))
                                 t
                                 $false)) ) ())
{
    for (n=1;!(n>100);n=n+1)
        if (f(n)<0.0)
            break;
t
.ft
.DE
.DS L
.ft CR
<1>: (gentran '( ((mcond) ((mgreaterp) n 100)
                          (($stop))
                          t
                          $false)
                 $end_group ) ())
    if (n>100.0)
        exit(0);
}
t
.ft
.DE
.LP
Translation is a convenient method of producing numerical code
when the exact behavior of the resultant code is known.  It gives
the VAXIMA user who is familiar with the syntax of
statements in the VAXIMA programming language the ability to
write code in a numerical programming language without knowing
the exact syntactical requirements of the language.  However,
the \fIreal\fR power of the \fIgentran\fR command lies in its ability to
generate code:  it can produce numerical code from symbolic
expressions derived in VAXIMA in addition to translating
statements directly.  This aspect is described in the next section.
.NH 2
Code Generation:  Evaluation Prior to Translation
.LP
Section 2.2 showed how VAXIMA statements and expressions can
be translated directly into the target language.  This section
shows how to indicate that parts of those statements and expressions
are to be handed to VAXIMA to be evaluated before being
translated.  In other words, this section explains how to generate
numerical code from algorithmic specifications (in the VAXIMA
programming language) and symbolic expressions.
.LP
Each of the following four subsections describes a special
function that can be used to request partial or
full evaluation of expressions prior to translation.  Note that
these functions have the described effects \fIonly\fR when used
as arguments to the \fIgentran\fR function.
.NH 3
The eval Function
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIeval(\fRexp)
.ft
.DE
.RE
where exp is any VAXIMA user level expression or statement which,
after evaluation by VAXIMA, results in an expression that can
be translated by \fIgentran\fR into the target language.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(($eval) \fRexp)
.ft
.DE
.RE
where exp is any VAXIMA LISP level expression or statement
which, after evaluation by VAXIMA, results in an expression
that can be translated by \fIgentran\fR into the target language.
.SH
Side Effects:
.LP
When \fIeval\fR is called on an argument which is to be translated,
it tells \fIgentran\fR to give the expression to VAXIMA for evaluation
first, and then to translate the result of that evaluation.
.SH
User Level Example:
.LP
The following formula, f, has been derived symbolically:
.RS
.DS L
.ft CR
   2
2 x  - 5 x + 6
.ft
.DE
.RE
We wish to generate an assignment statement for the quotient
of f and its derivative.
.DS L
.ft CR
(c1) gentran( q : eval(f)/eval(diff(f,x)) );
      q=(6.0-5.0*x+2.0*x**2)/(-5.0+4.0*x)

(d1)                       true
.ft
.DE
.SH
LISP Level Example:
.LP
The following formula, f, has been derived symbolically:
.RS
.DS L
.ft CR
((mplus) ((mtimes) 2 ((mexpt) x 2))
         ((mtimes) -5 x)
         6)
.ft
.DE
.RE
We wish to generate an assignment statement for the quotient of
f and its derivative.
.DS L
.ft CR
<1>: (gentran '(((msetq) q ((mquotient) (($eval) f)
                                        (($eval) (($diff) f x))))) ())
      q=(6.0-5.0*x+2.0*x**2)/(-5.0+4.0*x)
t
.ft
.DE
.NH 3
The rsetq Function
.LP
In many applications, assignments must be generated in which the
left-hand side is some known variable name, but the
right-hand side is an expression that must be evaluated.  For
this reason, a special function is provided to
indicate that the expression on the right-hand side is to be
evaluated prior to translation.  This special function is \fIrsetq\fR.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIrsetq(\fRvar, exp)
.ft
.DE
.RE
where var is any VAXIMA user level variable, matrix or array
element, and exp is any VAXIMA user level expression which,
after evaluation by VAXIMA, results in an expression that can
be translated by \fIgentran\fR into the target language.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(($rsetq) \fRvar exp)
.ft
.DE
.RE
where var is any VAXIMA LISP level variable, matrix or array
element, and exp is any VAXIMA LISP level expression which,
after evaluation by VAXIMA, results in an expression that can
be translated by \fIgentran\fR into the target language.
.SH
User Level Example:
.DS L
.ft CR
(c1) gentran( rsetq( deriv, diff(x^4-x^3+2*x^2-1,x) ) );
      deriv=4.0*x-3.0*x**2+4.0*x**3

(d1)                       true
.ft
.DE
Note:  This is equivalent to a call of the form
.RS
.DS L
.ft CR
gentran( deriv : eval(diff(x^4-x^3+2*x^2-1,x)) );
.ft
.DE
.RE
.SH
LISP Level Example:
.DS L
.ft CR
<1>: (gentran '((($rsetq) deriv
                          (($diff) ((mplus) ((mexpt) x 4)
                                            ((mminus) ((mexpt) x 3))
                                            ((mtimes) 2 ((mexpt) x 2))
                                            -1)
                                   x))) ())
      deriv=4.0*x-3.0*x**2+4.0*x**3
t
.ft
.DE
.NH 3
The lsetq Function
.LP
When assignments to matrix or array elements must be generated,
many times the indices of the element must be evaluated first.  The
special function \fIlsetq\fR can be used within a call to \fIgentran\fR
to indicate that the indices of the matrix or array element on
the left-hand side of the assignment are to be evaluated prior to
translation.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIlsetq(\fRvar, exp)
.ft
.DE
.RE
where var is any VAXIMA user level matrix or array element
with indices which, after evaluation by VAXIMA, will result in
expressions that can be translated by \fIgentran\fR, and exp is any
VAXIMA user level expression that can be translated into the
target language.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(($lsetq) \fRvar exp)
.ft
.DE
.RE
where var is any VAXIMA LISP level matrix or array element with
indices which, after evaluation by VAXIMA, result in expressions
that can be translated by \fIgentran\fR, and exp is any VAXIMA
LISP level expression that can be translated into the target language.
.SH
User Level Example:
.LP
We wish to generate assignments which assign zeros to all
elements on the main diagonal of m, an n x n matrix.
.DS L
.ft CR
(c1) for j:1 thru n do
         gentran( lsetq( m[j,j], 0.0 ) );
      m(1,1)=0.0
      m(2,2)=0.0
       .
       .
       .
      m(\fIn\fR,\fIn\fR)=0.0

(d1)                       done
.ft
.DE
Note:  This is equivalent to a call of the form
.DS L
.ft CR
for j:1 thru n do
    gentran( m[eval(j),eval(j)] : 0.0 );
.ft
.DE
.SH
LISP Level Example:
.LP
We wish to generate assignments which assign zeros to all
elements on the main diagonal of m, an n x n matrix.
.DS L
.ft CR
<1>: (do ((j 1 (1+ j)))
         ((> j n))
         (gentran '((($lsetq) ((m) j j) 0.0)) ()))
      m(1,1)=0.0
      m(2,2)=0.0
       .
       .
       .
      m(\fIn\fR,\fIn\fR)=0.0
nil
.ft
.DE
.NH 3
The lrsetq Function
.LP
In applications in which evaluated expressions are to be assigned to
matrix or array elements with evaluated subscripts, the \fIlrsetq\fR
function can be used.  It is a combination of the \fIlsetq\fR and
\fIrsetq\fR functions described in sections 2.3.2 and 2.3.3.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIlrsetq(\fRvar, exp)
.ft
.DE
.RE
where var is any VAXIMA user level matrix or array
element with indices which, after evaluation by VAXIMA,
will result in expressions that can be translated by \fIgentran\fR;
and exp is any user level expression which, after evaluation,
will result in an expression that can be translated by
\fIgentran\fR into the target language.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(($lrsetq) \fRvar exp)
.ft
.DE
.RE
where var is any VAXIMA LISP level matrix or array element
with indices which, after evaluation by VAXIMA, will result in
expressions that can be translated by \fIgentran\fR; and exp is any
LISP level expression which, after evaluation, will result in an
expression that can be translated by \fIgentran\fR into the target
language.
.SH
User Level Example:
.LP
The following matrix, m, has been derived symbolically:
.RS
.DS  L
.ft CR
[    a   0  - 1    1 ]
[                    ]
[    0   b    0    0 ]
[                    ]
[  - 1   0    c  - 1 ]
[                    ]
[    1   0  - 1    d ]
.ft
.DE
.RE
We wish to generate assignment statements for those elements
on the main diagonal of the matrix.
.DS L
.ft CR
(c1) for j:1 thru 4 do
         gentran( lrsetq( m[j,j], m[j,j] ) );
      m(1,1)=a
      m(2,2)=b
      m(3,3)=c
      m(4,4)=d

(d1)                       done
.ft
.DE
Note:  This is equivalent to a call of the form
.DS L
.ft CR
for j:1 thru 4 do
    gentran( m[eval(j),eval(j)] : eval(m[j,j]) );
.ft
.DE
.SH
LISP Level Example:
.LP
The following matrix, m, has been derived symbolically:
.RS
.DS L
.ft CR
[     a   0    1    1 ]
[                     ]
[     0   b    0    0 ]
[                     ]
[   - 1   0    c  - 1 ]
[                     ]
[     1   0  - 1    d ]
.ft
.DE
.RE
We wish to generate assignment statements for those elements on the main
diagonal of the matrix.
.DS L
.ft CR
<1>: (do ((j 1 (1+ j)))
         ((> j 4))
         (gentran '((($lrsetq) ((m array) j j)
                               ((m array) j j))) ()))
      m(1,1)=a
      m(2,2)=b
      m(3,3)=c
      m(4,4)=d
nil
.ft
.DE
.NH 2
Type Declarations
.LP
Type declarations are automatically generated each time a
subprogram heading is generated.  Type declarations are
constructed from information stored in the GENTRAN symbol
table.  The user can place entries into the symbol table
explicitly through calls to the special GENTRAN function \fItype\fR.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fItype(\fRtype, v1,v2,...,vn);
.ft
.DE
.RE
where v1,v2,...,vn is one or more variables (optionally
subscripted to indicate array dimensions), or variable ranges
(two letters separated by "-" and enclosed in double
quotes).  v's are not evaluated unless given as arguments to
\fIeval\fR.  type is a variable type in the target language.  It
should be a string or an atom.  type is not evaluated unless given as an
argument to \fIeval\fR.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(($type) \fRtype v1 v2 ... vn)
.ft
.DE
.RE
where v1 v2 ... vn is one or more variables (optionally in
lists with array dimensions), or variable ranges (two letters
separated by "-").  v's are not evaluated unless given as
arguments to \fI$eval\fR.  type is a variable type in the target
language.  It must be an atom.  type is not evaluated
unless given as an argument to \fI$eval\fR.
.SH
Side Effects:
.LP
Entries are placed in the symbol table for each variable or
variable range declared in the call to this function.  The
function call itself is removed from the statement group being
translated.  Then after translation, type declarations are generated
from these symbol table entries before the resulting executable
statements are printed.
.SH
User Level Example:
.DS L
.ft CR
(c1) gentran( type("implicit real*8", "a-h","o-z"),
              type("real*8", m(4,4)),
              for i:1 thru 4 do
                  for j:1 thru 4 do
                      if i=j
                         then m[i,j] : 1.0
                         else m[i,j] : 0.0,
              type("integer", i,j),
                .
                .
                .
            );
.ft
.DE
.DS L
.ft CR
      implicit real*8 (a-h,o-z)
      real*8 m(4,4)
      integer i,j
      do 25001 i=1,4
          do 25002 j=1,4
              if (.not.i.eq.j) goto 25003
                  m(i,j)=1.0
                  goto 25004
25003         continue
                  m(i,j)=0.0
25004         continue
25002     continue
25001 continue
       .
       .
       .
.ft
.DE
.DS L
.ft CR
(d1)                       true
.ft
.DE
.SH
LISP Level Example:
.DS L
.ft CR
<1>: (gentran '((($type) |implicit real*8| a-h o-z)
                (($type) real*8 ((m) 4 4))
                ((mdo) i 1 nil nil 4 nil
                       ((mdo) j 1 nil nil 4 nil
                              ((mcond) ((mequal) i j)
                                       ((msetq) ((m) i j) 1.0)
                                       t
                                       ((msetq) ((m) i j) 0.0))))
                (($type) integer i j)
                  .
                  .
                  .
               ) ())
      implicit real*8 (a-h,o-z)
      real*8 m(4,4)
      integer i,j
      do 25001 i=1,4
          do 25002 j=1,4
              if (.not.i.eq.j) goto 25003
                  m(i,j)=1.0
                  goto 25004
25003         continue
                  m(i,j)=0.0
25004         continue
25002     continue
25001 continue
       .
       .
       .
t
.ft
.DE
.NH 3
Subprogram Type Declarations
.LP
The \fItype\fR function can also be used to declare subprogram
types (i.e., subroutine or function) for FORTRAN and RATFOR
code, and function types for FORTRAN, RATFOR, and C code.
.SH
User Level Examples:
.DS L
.ft CR
(c1) gentranlang(ratfor)$
.ft
.DE
.DS L
.ft CR
(c2) gentran( fac(n) := block( type(function, fac),
                               type(integer, fac,n),
                               f : 1,
                               for i:2 thru n do
                                   f : f*i,
                               type(integer, f,i),
                               return(f) ) );
integer function fac(n)
integer n,f,i
f=1
do i=2,n
    f=f*i
return(f)
end

(d2)                       true
.ft
.DE
.DS L
.ft CR
(c3) gentranlang(c)$
.ft
.DE
.DS L
.ft CR
(c4) gentran( fac(n) := block( type(int, fac,n,i,f),
                               f : 1,
                               for i:2 thru n do
                                   f : f*i,
                               return(f) ) );
int fac(n)
int n;
{
    int i,f;
    f=1;
    for (i=2;!(i>n);i=i+1)
        f=f*i;
    return(f);
}

(d4)                       done
.ft
.DE
.SH
LISP Level Examples:
.DS L
.ft CR
<1>: (setq *gentranlang 'ratfor)
ratfor
.ft
.DE
.DS L
.ft CR
<1>: (gentran '(((mdefine) ((fac) n)
                           ((mprog)
                            (($type) function fac)
                            (($type) integer fac n)
                            ((msetq) f 1)
                            ((mdo) i 2 nil nil n nil
                                   ((msetq) f ((mtimes) f i)))
                            (($type) integer f i)
                            ((mreturn) f)))) ())
integer function fac(n)
integer n,f,i
f=1
do i=2,n
    f=f*i
return(f)
end
t
.ft
.DE
.DS L
.ft CR
<1>: (setq *gentranlang 'c)
c
.ft
.DE
.DS L
.ft CR
<1>: (gentran '(((mdefine) ((fac) n)
                           ((mprog)
                            (($type) int fac n i f)
                            ((msetq) f 1)
                            ((mdo) i 2 nil nil n nil
                                   ((msetq) f ((mtimes) f i)))
                            ((mreturn) f)))) ())
int fac(n)
int n;
{
    int i,f;
    f=1;
    for (i=2;!(i>2);i=i+1)
        f=f*i;
    return(f);
}
t
.ft
.DE
.NH 3
A Note on Subscripted Variables
.LP
When generating code for subscripted variables (i.e., matrix and
array elements), it is important to keep several things in
mind.  First of all, when a VAXIMA array is declared with a declaration
such as
.RS
.DS L
.ft CR
array(a, \fIn\fR);
.ft
.DE
.RE
where \fIn\fR is a positive integer, a is actually being declared to
be of size \fIn\fR+1.  Each of the elements a[0], a[1], ..., a[\fIn\fR]
can be used.  However, a FORTRAN or RATFOR declaration such as
.RS
.DS L
.ft CR
dimension a(\fIn\fR)
.ft
.DE
.RE
only declares a to be of size \fIn\fR.  Only the elements a(1),
a(2), ..., a(\fIn\fR) can be used.  Furthermore, a C declaration such as
.RS
.DS L
.ft CR
float a[\fIn\fR];
.ft
.DE
.RE
declares a to be of size \fIn\fR with elements referred to as
a[0], a[1], ..., a[\fIn\fR-1].
.LP
To resolve these array size and subscripting conflicts, the user
should remember the following:
.IP -
All VAXIMA array subscripts are translated literally.  Therefore
it is the user's responsibility to be sure that
array elements with subscript 0 are not translated into FORTRAN or
RATFOR.
.IP -
Since C arrays allow elements with a subscript of 0, when an array is
declared to be of size \fIn\fR by the user, the actual generated C type
declaration will be of size n+1 so that the user can translate elements with
subscripts from 0, up to and including \fIn\fR.
.NH 2
Comments and Literal Strings
.LP
Comments and other strings of characters can be inserted directly
into the stream of generated code through a call to the special function
\fIliteral\fR.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIliteral(\fRarg1,arg2,...,argn)
.ft
.DE
.RE
where arg1,arg2,...,argn is an argument list containing one or
more arg's, each of which either is, or evaluates to, an
atom.  The atoms tab and cr have special meanings.  arg's
are not evaluated unless given as arguments to \fIeval\fR.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(($literal) \fRarg1 arg2 ... argn)
.ft
.DE
.RE
where arg1 arg2 ... argn is an argument list containing one or
more arg's, each of which either is, or evaluates to, an
atom.  The atoms $tab and $cr have special meanings.  arg's
are not evaluated unless given as arguments to \fI$eval\fR.
.SH
Side Effect:
.LP
This function call is replaced by the character sequence resulting
from concatenation of the given atoms.  Double quotes are
stripped from all string type arg's, and each occurrence of
the reserved atom tab or cr ($tab or $cr at
the LISP level) is replaced by a tab to the current level of
indentation, or an end-of-line character.
.SH
User Level Examples:
.DS L
.ft CR
(c1) gentranlang(fortran)$
.ft
.DE
Suppose \fBn\fR has value 10.
.DS L
.ft CR
(c2) gentran( literal( "c", tab, "--This is a FORTRAN Comment--", cr,
                       "c", cr ),
              literal( tab, "data ", "n/", eval(n), "/", cr ) );
c     --This is a FORTRAN Comment--
c
      data n/10/

(d2)                       true
.ft
.DE
.DS L
.ft CR
(c3) gentranlang(ratfor)$
.ft
.DE
.DS L
.ft CR
(c4) gentran( for i:1 thru n do
              (
                  literal( tab, "# --This is a RATFOR Comment--", cr ),
                  literal( tab, "write(6,10) (m(i,j),j=1,n)", cr ),
                  literal( 10, tab, "format(1x,10(i5,3x))", cr )
              ) );
do i=1,n
    {
        # --This is a RATFOR Comment--
        write(6,10) (m(i,j),j=1,n)
10      format(1x,10(i5,3x))
    }

(d4)                       true
.ft
.DE
.DS L
.ft CR
(c5) gentranlang(c)$
.ft
.DE
.DS L
.ft CR
(c6) gentran( x : 0.0,
              literal( tab, "/*", cr,
                       tab, " * This is a C Comment", cr,
                       tab, " */", cr ) );
x=0.0;
/*
 * This is a C Comment
 */

(d6)                       true
.ft
.DE
.SH
LISP Level Examples:
.DS L
.ft CR
<1>: (setq *gentranlang 'fortran)
fortran
.ft
.DE
Suppose \fBn\fR has value 10.
.DS L
.ft CR
<1>: (gentran '((($literal) |c| $tab |--This is a FORTRAN Comment--| $cr
                            |c| $cr)
                (($literal) $tab |data | |n//| (($eval) n) |//| $cr)) ())
c     --This is a FORTRAN Comment--
c
      data n/10/
t
.ft
.DE
.DS L
.ft CR
<1>: (setq *gentranlang 'ratfor)
ratfor
.ft
.DE
.DS L
.ft CR
<1>: (gentran '(((mdo) i 1 nil nil n nil
                       ((mprogn)
                        (($literal) $tab |# --This is a RATFOR Comment--| $cr)
                        (($literal) $tab |write(6,10) (m(i,j),j=1,n)| $cr)
                        (($literal) 10 $tab |format(1x,10(i5,3x))| $cr)))) ())
do i=1,n
    {
        # --This is a RATFOR Comment--
        write(6,10) (m(i,j),j=1,n)
10      format(1x,10(i5,3x))
    }
t
.ft
.DE
.DS L
.ft CR
<1>: (setq *gentranlang 'c)
c
.ft
.DE
.DS L
.ft CR
<1>: (gentran '(((msetq) x 0.0)
                (($literal) $tab |//*| $cr
                            $tab | * This is a C Comment| $cr
                            $tab | *//| $cr)) ())
x=0.0;
/*
 * This is a C Comment
 */
t
.ft
.DE
.NH 2
The fortran, ratfor, and c Mode Switches
.LP
Sections 2.1 - 2.5 have shown how to produce numerical code
from derived expressions and algorithmic specifications.  First
the variable \fI*gentranlang\fR is set to \fIfortran\fR, \fIratfor\fR,
or \fIc\fR, and then the \fIgentran\fR function is called.  Any of the
special functions \fIeval\fR, \fIlsetq\fR, \fIrsetq\fR, \fIlrsetq\fR,
\fItype\fR, and \fIliteral\fR can be called from within a call to
\fIgentran\fR.  This section describes an alternative method of code
generation.
.LP
Three mode switches are provided in GENTRAN to change the default mode
of VAXIMA from evaluation to translation.  These switches are \fIfortran\fR,
\fIratfor\fR, and \fIc\fR, and can be turned on and off with the
special functions \fIon\fR and \fIoff\fR.  Each time a fresh VAXIMA
session is started up, the system is in evaluation mode.  It prints a prompt
on the user's terminal screen and waits for an expression or statement
to be entered.  When it reads in a ; or $, the preceding
expression is evaluated, a new prompt is printed, and the system waits
for the user to enter another expression or statement.  This mode can be
changed to translation mode by turning on one of the following
switches:  \fIfortran\fR, \fIratfor\fR, or \fIc\fR.  After one of
these switches is turned on, and until it is turned off, every
expression or statement entered by the user is translated into the
corresponding language just as if it had been given as an argument
to the \fIgentran\fR function.  Each of the special functions which
can be used from within a call to \fIgentran\fR can be used at the
top level until the switch is turned off.  Thus, the \fIfortran\fR,
\fIratfor\fR, and \fIc\fR mode switches are an alternative method of
code generation.
.SH
User Level Examples:
.DS L
.ft CR
(c1) m : genmatrix(m, 2,2, 1,1)$
.ft
.DE
.DS L
.ft CR
(c2) on(fortran);

     rsetq( minv, m^^(-1) );
      minv(1,1)=m(2,2)/(-m(1,2)*m(2,1)+m(1,1)*m(2,2))
      minv(1,2)=-m(1,2)/(-m(1,2)*m(2,1)+m(1,1)*m(2,2))
      minv(2,1)=-m(2,1)/(-m(1,2)*m(2,1)+m(1,1)*m(2,2))
      minv(2,2)=m(1,1)/(-m(1,2)*m(2,1)+m(1,1)*m(2,2))

     rsetq( d, determinant(m) );
      d=-m(1,2)*m(2,1)+m(1,1)*m(2,2)

     off(fortran);

(d2)                       done
.ft
.DE
.SH
LISP Level Examples:
.DS L
.ft CR
<1>: (meval '((msetq) m (($genmatrix) m 2 2 1 1)) )
(($matrix simp) ((mlist simp) ((m simp array) 1 1) ((m simp array
) 1 2)) ((mlist simp) ((m simp array) 2 1) ((m simp array) 2 2)))
.ft
.DE
.DS L
.ft CR
<1>: (on '(fortran))

     rsetq( minv, ?m^^(-1) );
      minv(1,1)=m(2,2)/(-m(1,2)*m(2,1)+m(1,1)*m(2,2))
      minv(1,2)=-m(1,2)/(-m(1,2)*m(2,1)+m(1,1)*m(2,2))
      minv(2,1)=-m(2,1)/(-m(1,2)*m(2,1)+m(1,1)*m(2,2))
      minv(2,2)=m(1,1)/(-m(1,2)*m(2,1)+m(1,1)*m(2,2))

     rsetq( d, determinant(?m) );
      d=-m(1,2)*m(2,1)+m(1,1)*m(2,2)

     off(fortran);
$done
.ft
.DE
.NH 2
Code Optimization
.LP
VAXIMA contains an \fIoptimize\fR command which takes one expression
as its argument.  It searches for common subexpressions, replaces
them by temporary variable names, and returns the resulting expression
preceded by a sequence of assignment statements which assign the common
subexpressions to the temporary variable names.  GENTRAN has been
interfaced with this command to make it possible to generate code
in which the numerical expressions have been optimized.  This interface
is activated when the \fIgentranopt\fR flag is on.  (\fIgentranopt\fR is
off initially.)  The following command will turn the \fIgentranopt\fR flag
on:
.RS
.DS L
.ft CR
\fIon(gentranopt);\fR
.ft
.DE
.RE
and
.RS
.DS L
.ft CR
\fIoff(gentranopt);\fR
.ft
.DE
.RE
will turn it back off again.
.SH
Example:
.LP
Suppose the following expression, ans, has been derived:
.DS L
.ft CR
(c10) ans;

            2  7  4  8      2  6  3  8       3  7  4  6      3  6  3  6
(d10) - 36 w  x  y  z  + 3 w  x  y  z  - 24 w  x  y  z  + 2 w  x  y  z

       2  8  6  5        4  7  6  5       2  7  6  5        2  10  5  5
 + 96 w  x  y  z  - 168 w  x  y  z  + 12 w  x  y  z  - 216 w  x   y  z

      2  7  5  5      7  5  5       4  6  5  5    2  6  5  5       2  9  4  5
 - 8 w  x  y  z  + 9 x  y  z  + 14 w  x  y  z  - w  x  y  z  + 18 w  x  y  z

       7  3  5      2  6  3  5        7  5  3         7  3  3      3  6  3  3
 + 87 x  y  z  - 3 w  x  y  z  + 6 w x  y  z  + 58 w x  y  z  - 2 w  x  y  z

       8  7  2       2  7  7  2      7  7  2       10  6  2        8  5  2
 - 24 x  y  z  + 42 w  x  y  z  - 3 x  y  z  + 54 x   y  z  - 232 x  y  z

        2  7  5  2       7  5  2       4  6  5  2    2  6  5  2        10  4  2
 + 414 w  x  y  z  - 29 x  y  z  - 14 w  x  y  z  + w  x  y  z  + 522 x   y  z

       2  9  4  2
 - 18 w  x  y  z
.ft
.DE
We wish to generate optimized FORTRAN code to compute this value.
.LP
The following VAXIMA session will generate the code:
.DS L
.ft CR
(c11) on(gentranopt);

(d11)                      done
.ft
.DE
.DS L
.ft CR
(c12) gentran( rsetq(ans,ans) );
.ft
.DE
.DS L
.ft CR
      t0=w**2
      t1=x**9
      t2=y**4
      t3=z**2
      t4=x**10
      t5=x**6
      t6=y**5
      t7=w**4
      t8=x**7
      t9=x**8
      t10=y**6
      t11=y**7
      t12=w**3
      t13=y**3
      t14=z**3
      t15=z**5
      t16=z**6
      t17=z**8
.ft
.DE
.DS L
.ft CR
      ans=-18.0*t0*t1*t2*t3+522.0*t4*t2*t3+t0*t5*t6*t3-14.0*t7*t5*t6*t3
     . -29.0*t8*t6*t3+414.0*t0*t8*t6*t3-232.0*t9*t6*t3+54.0*t4*t10*t3
     . -3.0*t8*t11*t3+42.0*t0*t8*t11*t3-24.0*t9*t11*t3-2.0*t12*t5*t13*
     . t14+58.0*w*t8*t13*t14+6.0*w*t8*t6*t14-3.0*t0*t5*t13*t15+87.0*t8*
     . t13*t15+18.0*t0*t1*t2*t15-t0*t5*t6*t15+14.0*t7*t5*t6*t15+9.0*t8*
     . t6*t15-8.0*t0*t8*t6*t15-216.0*t0*t4*t6*t15+12.0*t0*t8*t10*t15
     . -168.0*t7*t8*t10*t15+96.0*t0*t9*t10*t15+2.0*t12*t5*t13*t16-24.0*
     . t12*t8*t2*t16+3.0*t0*t5*t13*t17-36.0*t0*t8*t2*t17
.ft
.DE
.DS L
.ft CR
(d12)                      true
.ft
.DE
.LP
Unfortunately, VAXIMA's \fIoptimize\fR facility is not as powerful
as it could be.  It does not produce an absolutely
optimal sequence of assignments to calculate an
expression's value.  For example, in the code above,
.RS
.DS L
.ft CR
      t3=z**2
       .
       .
       .
      t14=z**3
      t15=z**5
      t16=z**6
      t17=z**8
.ft
.DE
.RE
could be calculated more efficiently with
.RS
.DS L
.ft CR
      t3=z*z
       .
       .
       .
      t14=t3*z
      t15=t3*t14
      t16=t15*z
      t17=t16*t3
.ft
.DE
.RE
In addition, the \fIoptimize\fR facility does not detect all possible
common subexpressions.  For example, in the above,
t2*t3 appears in each of the first two terms of the final
assignment statement.  That expression should have been replaced by
another temporary variable name, and
another assignment statement should have been generated for it.
.LP
A code optimizer, designed by J. A. van Hulzen /14/ of Twente University of
Technology in The Netherlands, has been implemented in RLISP to run under
REDUCE.  It utilizes an optimization technique in which one or more expressions
are mapped onto matrices of coefficients and exponents.  These matrices are
searched for patterns corresponding to common subexpressions, all of which
are replaced by temporary variable names.  This optimization technique is
much more powerful than the one used in VAXIMA's \fIoptimize\fR command.  A
comparison of code generated using each of the two optimization facilities
is given in sections 6.3.3 - 6.3.6.
.NH 2
Expression Segmentation
.LP
Symbolic derivations can easily produce formulas that can be
anywhere from a few lines to several pages in length.  Such
formulas can be translated into numerical assignment statements,
but unless they are broken into smaller pieces, they
may be too long for a compiler to handle.  (The maximum
number of continuation lines for one statement allowed by most
FORTRAN compilers is only 19.)  Therefore GENTRAN contains
a segmentation facility which automatically "segments", or
breaks down, unreasonably large expressions.
.LP
The segmentation facility generates a sequence of assignment
statements, each of which assigns a subexpression to an
automatically generated temporary variable.  This sequence
is generated in such a way that temporary variables are re-used
as soon as possible, thereby keeping the number of
automatically generated variables to a minimum.  The facility
can be turned on or off by setting the mode switch
\fIgentranseg\fR accordingly (i.e., by calling the function
\fIon\fR or \fIoff\fR on it).  The user can control the maximum allowable
expression size by setting the variable
\fI?maxexpprintlen\e*\fR to the maximum number of characters allowed
in an expression printed in the target language (excluding
spaces automatically printed by the formatter).  The
\fIgentranseg\fR switch is on initially, and \fI?maxexpprintlen\e*\fR is
initialized to 800.
.SH
User Level Examples:
.DS L
.ft CR
(c1) on(gentranseg)$
.ft
.DE
.DS L
.ft CR
(c2) ?maxexpprintlen\e* : 120$
.ft
.DE
.DS L
.ft CR
(c3) ( poly : 0,
       for i:0 thru 20 do
           poly : poly + c(i+1)*x^i )$
.ft
.DE
.DS L
.ft CR
(c4) gentran( rsetq( poly, poly ) );
      t0=c(1)+c(2)*x+c(3)*x**2+c(4)*x**3+c(5)*x**4+c(6)*x**5+c(7)*x**6+c
     . (8)*x**7+c(9)*x**8+c(10)*x**9
      t0=t0+c(11)*x**10+c(12)*x**11+c(13)*x**12+c(14)*x**13+c(15)*x**14+
     . c(16)*x**15+c(17)*x**16+c(18)*x**17
      poly=t0+c(19)*x**18+c(20)*x**19+c(21)*x**20

(d4)                       true
.ft
.DE
.SH
LISP Level Examples:
.DS L
.ft CR
<1>: (on '(gentranseg))
$done
.ft
.DE
.DS L
.ft CR
<1>: (setq maxexpprintlen* 120)
120
.ft
.DE
.DS L
.ft CR
<1>: (meval '((mprogn)
              ((msetq) poly 0)
              ((mdo) i 0 nil nil 20 nil
                     ((msetq) poly ((mplus) poly
                                            ((mtimes) ((c) ((mplus) i 1))
                                                      ((mexpt) x i)))) ) ) )
$done
.ft
.DE
.DS L
.ft CR
<1>: (gentran '((($rsetq) poly poly)) ())
      t0=c(1)+c(2)*x+c(3)*x**2+c(4)*x**3+c(5)*x**4+c(6)*x**5+c(7)*x**6+c
     . (8)*x**7+c(9)*x**8+c(10)*x**9
      t0=t0+c(11)*x**10+c(12)*x**11+c(13)*x**12+c(14)*x**13+c(15)*x**14+
     . c(16)*x**15+c(17)*x**16+c(18)*x**17
      poly=t0+c(19)*x**18+c(20)*x**19+c(21)*x**20
t
.ft
.DE
.NH 3
Implicit Type Declarations
.LP
When the segmentation routine generates temporary variables,
it places type declarations in the symbol table for those
variables if possible.  It uses the following rules to
determine their type:
.IP 1)
If the type of the variable to which the large expression
is being assigned is already known (i.e., has been
declared by the user), then the temporary variables will be
declared to be of that same type.
.IP 2)
If the global variable \fI?tempvartype\e*\fR (\fI$tempvartype\fR at the LISP
level) has a non-nil value, then the temporary variables are declared to
be of that type.
.IP 3)
Otherwise, the variables are not declared.
.SH
Example:
.DS L
.ft CR
(c1) on(gentranseg)$
.ft
.DE
.DS L
.ft CR
(c2) ?maxexpprintlen\e* : 20$
.ft
.DE
.DS L
.ft CR
(c3) ?tempvartype\e* : real$
.ft
.DE
.DS L
.ft CR
(c4) gentran( type(integer, isum),
               .
               .
              isum : i(1)+i(2)+i(3)+i(4)+i(5),
              pprod : p(1)*p(2)*p(3)*p(4)*p(5),
               .
               .
            );
      integer isum,t0
      real t1
       .
       .
      t0=i(1)+i(2)+i(3)+i(4)
      isum=t0+i(5)
      t1=p(1)*p(2)*p(3)*p(4)
      pprod=t1*p(5)
       .
       .

(d4)                       true
.ft
.DE
.NH 3
Controlling Generation of Declarations
.LP
In some applications, it may not be convenient to have type declarations
generated automatically.  The \fIgendecs\fR flag has been provided for
this reason.
.LP
\fIgendecs\fR is on initially.  As long as it stays on, all type
declarations will be generated whenever possible.  However, when
\fIgendecs\fR is off, type declarations will not be generated.  Instead,
type information will be stored in GENTRAN's Symbol Table but will not
be retrieved in the form of declarations unless and until either the
\fIgendecs\fR function is called or the \fIgendecs\fR flag is turned back
on.
.LP
The \fIgendecs\fR function can be called any time the \fIgendecs\fR flag
is turned off to retrieve all type declarations from the Symbol Table
for the given subprogram name (or the "current" subprogram if \fIfalse\fR or
\fInil\fR is given as its argument).  The syntax for calls to this function
is:
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIgendecs(\fRname);
.ft
.DE
.RE
where name is an atom.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(gendecs '\fRname)
.ft
.DE
.RE
where name is an atom.
.LP
The \fIgendecs\fR flag can be turned on and off with the \fIon\fR and
\fIoff\fR functions:
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIon(gendecs);\fR
.ft
.DE
.DS L
.ft CR
\fIoff(gendecs);\fR
.ft
.DE
.RE
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(on '(gendecs))\fR
.ft
.DE
.DS L
.ft CR
\fI(off '(gendecs))\fR
.ft
.DE
.RE
.LP
The \fIgendecs\fR flag will is discussed in connection with template
processing in section 3.2.
.NH 2
Generation of Temporary Variable Names
.LP
As we have just seen, GENTRAN's segmentation module generates
temporary variables and places type declarations in
the symbol table for them whenever possible.  Various other
modules also generate variables and corresponding declarations.  All
of these modules call one special GENTRAN function each
time they need a temporary variable name.  This
function is \fItempvar\fR.  There are situations in which it may
be convenient for the user to be able to generate temporary
variable names directly[3].  Therefore
.FS
[3] One such example is suppression of the simplification process to
generate numerical code which is more efficient.  See
the example in section 6.3.4.
.FE
\fItempvar\fR is a user-accessible function.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fItempvar(\fRtype);
.ft
.DE
.RE
where type is either a string which indicates the variable
type in the target language (e.g., "integer", "real*8", etc.), or
is \fIfalse\fR if the variable type is unknown.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(tempvar '\fRtype)
.ft
.DE
.RE
where type is either an atom which indicates the variable
type in the target language (e.g., integer, real*8, etc.), or
is \fInil\fR if the variable type is unknown.
.SH
Side Effects
.LP
\fItempvar\fR creates temporary variable names by repeatedly
concatenating the values of the global variables
\fI?tempvarname\e*\fR (\fItempvarname*\fR), which has a default value
of '\fIt\fR,
and \fI?tempvarnum\e*\fR (\fItempvarnum*\fR), which is initially set to 0,
and incrementing \fI?tempvarnum\e*\fR
until a variable name is created which satisfies
one of the following conditions:
.IP 1)
It had not been generated previously, and it has not been declared
by the user.
.IP 2)
It had previously been generated to hold the same type
of value that it must hold this time (e.g., integer, real, etc.),
and the value assigned to it previously is no longer needed.
.LP
If type is a non-\fIfalse\fR (non-\fInil\fR) argument, or if
type is \fIfalse\fR (\fInil\fR) and the global variable
\fI?tempvartype\e*\fR (\fItempvartype*\fR), which is initially \fIfalse\fR
(\fInil\fR), has been set to a non-\fIfalse\fR (non-\fInil\fR) value,
then a type entry for the generated variable name is placed in the
symbol table.
.SH
Returned Value:
.LP
\fItempvar\fR returns an atom which can be used as a variable.
.SH
Note:
.LP
It is the user's responsibility to set \fI?tempvarname\e*\fR
and \fI?tempvarnum\e*\fR (\fItempvarnum*\fR) to
values such that generated variable names will not clash with variables
used elsewhere in the program unless those variables have been
declared.
.NH 3
Marking Temporary Variables
.LP
In section 2.9 we saw that a temporary variable name (of a
certain type) can be regenerated when the value previously
assigned to it is no longer needed.  This section describes
a function which "marks" a variable to indicate that it
currently holds a significant value, and the next section
describes functions which "unmark" variables to indicate
that the values they hold are no longer significant.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fImarkvar(\fRvar);
.ft
.DE
.RE
where var is an atom.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(markvar \fRvar)
.ft
.DE
.RE
where var is an atom.
.SH
Side Effect:
.LP
\fImarkvar\fR sets a flag on var's property list to
indicate that var currently holds a significant value.
.SH
Return Value:
.LP
\fImarkvar\fR returns var.
.SH
User Level Example:
.LP
The following matrix, m, has been derived symbolically:
.RS
.DS L
.ft CR
[ x (z + y)         0     x z ]
[                             ]
[       - x     y + x       0 ]
[                             ]
[                           2 ]
[       x z         0      z  ]
.ft
.DE
.RE
We wish to replace each non-zero element by a generated variable name
to prevent these expressions from being resubstituted into further
calculations.  (We will also record these substitutions in the numerical
program we are constructing by generating assignment statements.)
.DS L
.ft CR
(c1) for i:1 thru 3 do
         for j:1 thru 3 do
             if m[i,j]#0 then
             (
                 var : tempvar(false),
                 markvar(var),
                 gentran( eval(var) : eval(m[i,j]) ),
                 m[i,j] : var
             );
.ft
.DE
.DS L
.ft CR
      t0=x*(y+z)
      t1=x*z
      t2=-x
      t3=x+y
      t4=x*z
      t5=z**2
.ft
.DE
.DS L
.ft CR
(d1)                       done
.ft
.DE
Now matrix m contains the following entries:
.RS
.DS L
.ft CR
[ t0     0    t1 ]
[                ]
[ t2    t3     0 ]
[                ]
[ t4     0    t5 ]
.ft
.DE
.RE
.SH
LISP Level Example:
.LP
The following matrix, m, has been derived symbolically:
.RS
.DS L
.ft CR
[ x (z + y)         0     x z ]
[                             ]
[       - x     y + x       0 ]
[                             ]
[                           2 ]
[       x z         0      z  ]
.ft
.DE
.RE
We wish to replace each non-zero element by a generated variable
name to prevent these expressions from being resubstituted into further
calculations.  (We will also record these substitutions in the
numerical program we are constructing by generating assignment
statements.)
.DS L
.ft CR
<1>: (meval '((mdo) i 1 nil nil 3 nil
                    ((mdo) j 1 nil nil 3 nil
                           ((mcond) ((mnotequal) ((m array) i j) 0)
                                    ((mprogn)
                                     ((msetq) var ((tempvar) nil))
                                     ((markvar) var)
                                     ((gentran)
                                      '(((msetq) (($eval) var)
                                                 (($eval) ((m array) i j)))) ())
                                     ((msetq) ((m array) i j) var))
                                    t
                                    $false))))
.ft
.DE
.DS L
.ft CR
      t0=x*(y+z)
      t1=x*z
      t2=-x
      t3=x+y
      t4=x*z
      t5=z**2
.ft
.DE
.DS L
.ft CR
$done
.ft
.DE
.NH 3
Unmarking Temporary Variables
.LP
After the value assigned to a temporary variable has been
used in the numerical program and is no longer needed, the
variable name can be "unmarked" with the \fIunmarkvar\fR
function.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIunmarkvar(\fRvar);
.ft
.DE
.RE
where var is an atom.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(unmarkvar \fRvar)
.ft
.DE
.RE
where var is an atom.
.SH
Side Effect:
.LP
\fIunmarkvar\fR resets a flag on var's property list to indicate
that var does not hold a significant value.
.LP
.LP
The \fIrecurunmark\fR function unmarks all variable names in an
expression.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIrecurunmark(\fRexp);
.ft
.DE
.RE
where exp is an expression containing one or more variable
names.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(recurunmark \fRexp)
.ft
.DE
.RE
where exp is an expression containing one or more variable
names.
.SH
Side Effect:
.LP
\fIrecurunmark\fR resets flags on the property lists of all
variable names in exp to indicate that they do not hold
significant values any longer.
.NH 3
The Marked Variable Predicate
.LP
A variable name can be tested to see if it is currently marked
with the marked variable predicate.
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fImarkedvarp(\fRvar);
.ft
.DE
.RE
where var is an atom.
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(markedvarp '\fRvar)
.ft
.DE
.RE
where var is an atom.
.SH
Return Value:
.LP
\fImarkedvarp\fR returns \fItrue\fR or \fIfalse\fR (\fIt\fR
or \fInil\fR at the LISP level), depending
on whether or not var is currently marked.
.NH 2
Generation of Statement Numbers
.LP
The \fIgenstmtno\fR function is called by GENTRAN whenever a new
statement number must be generated.  The syntax is as follows:
.SH
User Level Syntax:
.RS
.DS L
.ft CR
\fIgenstmtno();\fR
.ft
.DE
.RE
.SH
LISP Level Syntax:
.RS
.DS L
.ft CR
\fI(genstmtno)\fR
.ft
.DE
.RE
.SH
Side Effects:
.LP
\fIgenstmtno\fR increases the value of global variable
\fI?genstmtno\e*\fR (\fIgenstmtno*\fR) by \fI?genstmtincr\e*\fR
(\fIgenstmtincr*\fR) when it is called.  (\fI?genstmtno\e*\fR is
initialized to 25000, and \fI?genstmtincr\e*\fR is initialized to 1.)
.SH
Return Value:
\fIgenstmtno\fR returns the new value of \fI?genstmtno\e*\fR
(\fIgenstmtno*\fR).
