.nr LL 6.5i
.nr FL 6.0i
.ND
.SH
A.  TRANSLATABLE VAXIMA STATEMENTS & EXPRESSIONS
.LP
A substantial subset of all VAXIMA statements and expressions can be
translated by GENTRAN into semantically equivalent code in the target
numerical language[9].  This
.FS
[9] It should be noted that call-by-value parameter passing is used in
VAXIMA, while call-by-address parameter passing is normally used in
FORTRAN and RATFOR.  GENTRAN does not attempt to simulate call-by-value
passing when generating code.
.FE
appendix is divided into two sections.  The first section contains a
formal definition of all translatable VAXIMA statements and expressions.  The
second section contains examples of several statement types
translated into FORTRAN, RATFOR and C code.
.SH
A.1.  Formal Definition
.LP
This section contains a formal definition of all VAXIMA statements and
expressions that can be translated by GENTRAN into numerical
code.  First the VAXIMA user level syntax is given, and then the
LISP level syntax is given.
.LP
Note:  Terminals are \fIunderlined\fR.
.LP
       \fIeps\fR  represents the empty string.
.SH
User Level
.RS
.SH
Preliminary Definitions
.LP
\fIid\fR     - an identifier (i.e., atom).
.br
\fIstring\fR - any number of characters (excluding double quotes)
.br
         enclosed in double quotes.
.SH
Expressions
.LP
Arithmetic Expressions:
.RS
.LP
exp  ::=  \fInumber\fR  |  var  |  funcall |  \fIabs( \fRexp )  |
.br
          - exp  |  exp + exp  |  exp - exp  |
.br
          exp * exp  |  exp / exp  |  exp ** exp  |
.br
          exp ^ exp  |  ( exp )
.LP
exp'  ::=  , exp exp'  |  \fIeps\fR
.LP
exp''  ::=  exp exp'  |  \fIeps\fR
.LP
var  ::=  \fIid\fR  |  \fIid [ \fRexp exp' ]
.LP
funcall  ::=  \fIid( \fRarg' )
.LP
arg  ::=  exp  |  logexp  |  \fIstring\fR
.LP
arg'  ::=  arg arg'  |  \fIeps\fR
.LP
arg0  ::=  arg  |  \fIeps\fR
.LP
string0  ::=  \fIstring\fR  |  \fIeps\fR
.LP
id'  ::=  \fIid\fR id''  |  \fIeps\fR
.LP
id''  ::=  , \fIid\fR id''  |  \fIeps\fR
.RE
.LP
Logical Expressions:
.RS
.LP
logexp  ::=  \fItrue\fR  |  \fIfalse\fR  |  var  |  funcall  |
.br
             exp > exp  |  exp >= exp  |  exp = exp  |
.br
             exp # exp  |  exp < exp  |  exp <= exp  |
.br
             \fInot\fR logexp  |  logexp \fIand\fR logexp  |
.br
             logexp \fIor\fR logexp  |  ( logexp )
.RE
.SH
Operator Precedence
.LP
Parentheses are automatically generated when expressions are
translated which contain operators whose precedence in VAXIMA
differs from that in the target language.  Thus the meaning of
the original expression is preserved.
.LP
For example, in VAXIMA,
.DS L
.ft CR
    not a = b    and    not (a = b)
.ft
.DE
are equivalent, whereas in C,
.DS L
.ft CR
    ! a == b     and    (!a) == b
.ft
.DE
are equivalent.  Therefore,
.DS L
.ft CR
    not a = b
.ft
.DE
is translated into C code which forces the VAXIMA precedence rules:
.DS L
.ft CR
    !(a == b)
.ft
.DE
.SH
Statements
.RS
.LP
stmt  ::=  assign  |  loop  |  cond  |  go  |  call  |
.br
            return  |  iostmt  |  stmtgp
.LP
stmt'  ::=  , stmt stmt'  |  \fIeps\fR
.LP
assign  ::=  var : exp  |
\fIid : matrix( \fRrow row' )
.LP
row  ::=  [ exp exp' ]
.LP
row'  ::=  , row row'  |  \fIeps\fR
.LP
loop  ::=  \fIfor\fR var initval nextexp thruexp loopcond \fIdo\fR
.br
           stmt
.LP
initvar  ::=  : exp  |  \fIeps\fR
.LP
nextexp  ::=  \fIstep\fR exp  |  \fInext\fR exp  |  \fIeps\fR
.LP
thruexp  ::=  \fIthru\fR exp  |  \fIeps\fR
.LP
loopcond  ::=  \fIwhile\fR logexp  |  \fIunless\fR logexp  |  \fIeps\fR
.LP
cond  ::=  \fIif\fR logexp \fIthen\fR stmt elsestmt
.LP
elsestmt  ::=  \fIelse\fR stmt  |  \fIeps\fR
.LP
go  ::=  \fIgo( \fRid )
.LP
call  ::=  \fIid( \fRexp'' )
.LP
return  ::=  \fIreturn( \fRarg0 )
.LP
iostmt[10]  ::=  var : \fBreadonly( \fRstring0 )  |
.br
                 \fIprint( \fRarg arg' )
.FS
[10] I/O statements cannot be translated into C code.
.FE
.LP
stmtgp  ::=  ( stmt stmt' )[11]  |
.br
             \fIblock( \fRstmt stmt' )[12]
.FS
[11] VAXIMA compound statements are translated into (undelimited) statement
\fIsequences\fR.
.FE
.FS
[12] VAXIMA statement blocks are translated into statement \fIgroups\fR
(delimited by { and }) when RATFOR or C code is being generated.  However,
they are translated into (undelimited) statement \fIsequences\fR when the
target language is FORTRAN.
.FE
.RE
.SH
Subprograms
.RS
.LP
subprog  ::=  \fIid( \fRid' ) := body ;
.LP
body  ::=  stmt  |  exp  |  logexp
.RE
.RE
.SH
LISP Level
.RS
.SH
Preliminary Definitions
.LP
\fIid     \fR- an identifier (i.e., atom whose first character is
.br
         not &).
.br
\fIstring\fR - an atom whose first character is &.
.SH
Expressions
.LP
Arithmetic Expressions:
.RS
.LP
exp  ::=  \fInumber\fR  |  var  |  funcall  |
.br
          \fI((mabs) \fRexp)  |  \fI((mminus) \fRexp)  |
.br
          \fI((mplus) \fRexp exp')  |  \fI((mtimes) \fRexp exp')  |
.br
          \fI((mquotient) \fRexp exp)  |  \fI((mexpt) \fRexp exp)
.LP
exp'  ::=  exp exp'  |  \fIeps\fR
.LP
var  ::=  \fIid\fR  |  \fI((id) \fRexp exp')
.LP
funcall  ::=  \fI((id) \fRarg')
.LP
arg  ::=  exp  |  logexp  |  \fIstring\fR
.LP
arg'  ::=  arg arg'  |  \fIeps\fR
.LP
arg0  ::=  arg  |  \fIeps\fR
.LP
string0  ::=  \fIstring\fR  |  \fIeps\fR
.LP
id'  ::=  \fIid \fRid'  |  \fIeps\fR
.RE
.LP
Logical Expressions:
.RS
.LP
logexp  ::=  \fIt\fR  |  \fInil\fR  |  var  |  funcall  |
.br
             \fI((mgreaterp) \fRexp exp)  |
.br
             \fI((mgeqp) \fRexp exp)  |
.br
             \fI((mequal) \fRexp exp)  |
.br
             \fI((mnotequal)\fR exp exp)  |
.br
             \fI((mlessp) \fRexp exp)  |
.br
             \fI((mleqp) \fRexp exp)  |  \fI((mnot) \fRlogexp)  |
.br
             \fI((mand) \fRlogexp logexp logexp')  |
.br
             \fI((mor) \fRlogexp logexp logexp')
.LP
logexp'  ::=  logexp logexp'  |  \fIeps\fR
.RE
.SH
Statements
.RS
.LP
stmt  ::=  assign |  loop  |  condR  |  go  |  call  |
.br
           return  |  iostmt  |  stmtgp
.LP
stmt'  ::=  stmt stmt'  |  \fIeps\fR
.LP
assign  ::=  \fI((msetq) \fRvar exp)  |
.br
             \fI((msetq) id (($matrix) \fRrow row'))
.LP
row  ::=  \fI((mlist) \fRexp exp')
.LP
row'  ::=  row row'  |  \fIeps\fR
.LP
loop  ::=  \fI((mdo) \fRvar initexp stepexp nextexp thruexp
.br
           loopcond stmt)
.LP
initexp  ::=  exp  |  \fInil\fR
.LP
stepexp  ::=  exp  |  \fInil\fR
.LP
nextexp  ::=  exp  |  \fInil\fR
.LP
thruexp  ::=  exp  |  \fInil\fR
.LP
loopcond  ::=  logexp  |  \fInil\fR
.LP
cond  ::=  \fI((mcond) \fRlogexp stmt \fIt\fR elsestmt)
.LP
elsestmt  ::=  stmt  |  \fI$false\fR
.LP
go  ::=  \fI((mgo) id)\fR
.LP
call  ::=  \fI((id) \fRexp')
.LP
return  ::=  \fI((mreturn) \fRarg0)
.LP
iostmt[13]  ::=  \fI((msetq) \fRvar \fI(($readonly) \fRstring0))  |
.br
.FS
[13] I/O statements cannot be translated into C code.
.FE
                 \fI(($print) \fRarg arg')
.LP
stmtgp  ::=  \fI((mprogn) \fRstmt stmt')[14]  |
.br
              \fI((mprog) \fRstmt stmt')[15]
.FS
[14] VAXIMA compound statements are translated into (undelimited) statement
\fIsequences\fR.
.FE
.FS
[15] VAXIMA statement blocks are translated into statement groups (delimited
by { and }) when RATFOR or C code is being generated.  However, they are
translated into (undelimited) statement \fIsequences\fR when the
target language is FORTRAN.
.FE
.RE
.SH
Subprograms
.RS
.LP
subprog  ::=  \fI((mdefine) ((id) \fRid') body)
.LP
body  ::=  stmt  |  exp  |  logexp
.RE
.RE
.SH
A.2.  Examples
.LP
The following table contains a list of VAXIMA statement types
that can be translated by GENTRAN.  Examples of each statement
type, along with equivalent FORTRAN, RATFOR and C code, are given.
.bp
 .
.bp
 .
