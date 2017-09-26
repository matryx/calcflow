.SH
B.  LIST OF GENTRAN FUNCTIONS, SWITCHES AND VARIABLES
.DS L
.ft CR
User Level Syntax               | LISP Level Syntax
--------------------------------+----------------------------------------------
.ft
.DE
.DS L
.ft CR
Functions:                      | Functions:
                                |
  \fIgentran(\fRstmt1,stmt2,...,stmtn |   (\fIgentran '(\fRstmt1 stmt2 ...
          {,[f1,f2,...,fm]});   |                          stmtn)
                           [16] |            '(f1 f2 ... fm))[16]
  \fIgentranin(\fRf1,f2,...,fn        |   (\fIgentranin '(\fRf1 f2 ... fn)
            {,[f1,f2,...,fm]}); |              '(f1 f2 ... fm))
  \fIgentranout(\fRf1,f2,...,fn);     |   \fI(gentranout '(\fRf1 f2 ... fn))
  \fIgentranpop(\fRf1,f2,...,fn);     |   \fI(gentranpop '(\fRf1 f2 ... fn))
  \fIgentranpush(\fRf1,f2,...,fn);    |   \fI(gentranpush '(\fRf1 f2 ... fn))
  \fIgentranshut(\fRf1,f2,...,fn);    |   \fI(gentranshut '(\fRf1 f2 ... fn))
.ft
.DE
.FS
[16] In addition to the subset of translatable VAXIMA
statements given in Appendix A, the following special functions
and arguments can also be given as arguments to the
\fIgentran\fR function:
.DS L
.ft CR
  \fIbegin_group\fR                   |   \fI$begin_group\fR
  \fIbody(\fRstmt1,stmt2,...,stmtn)   |   \fI(($body) \fRstmt1 stmt2 ...
                                |                          stmtn)
  \fIbreak()\fR                       |   \fI(($break))\fR
  \fIcprocedure({\fRtype,}            |   \fI(($cprocedure) {\fRtype}
             name(p1,p2,...,pn))|                  ((name) p1 p2
                                |                        ... pn))
  \fIend()\fR                         |   \fI(($end))\fR
  \fIend_group\fR                     |   \fI$end_group\fR
  \fIeval(\fRexp)                     |   \fI(($eval) \fRexp)
  \fIfunction({\fRtype,}              |   \fI(($function) {\fRtype}
           name(p1,p2,...,pn))  |                ((name) p1 p2
                                |                        ... pn))
  \fIliteral(\fRarg1,arg2,...,argn)   |   \fI(($literal) \fRarg1 arg2 ...
                                |                           argn)
  \fIlrsetq(\fRvar,exp)               |   \fI(($lrsetq) \fRvar exp)
  \fIlsetq(\fRvar,exp)                |   \fI(($lsetq) \fRvar exp)
  \fIrsetq(\fRvar,exp)                |   \fI(($rsetq) \fRvar exp)
  \fIstop()\fR                        |   \fI(($stop))\fR
  \fIsubroutine(\fRname(p1,p2,...,pn))|   \fI(($subroutine) ((\fRname) p1 p2
                                |                        ... pn))
  \fItype(\fRtype, v1,v2,...,vn)      |   \fI(($type) \fRtype v1 v2 ... vn)
.ft
.DE
.FE
.DS L
.ft CR
  \fIoff(\fRflag1,flag2,...,flagn);   |   \fI(off '(\fRflag1 flag2 ...
                                |                         flagn))
  \fIon(\fRflag1,flag2,...,flagn);    |   \fI(on '(\fRflag1 flag2 ... flagn))
.ft
.DE
.DS L
.ft CR
  \fIgendecs(\fRname);                |   \fI(gendecs \fR'name)
  \fIgenstmtno();\fR                  |   \fI(genstmtno)\fR
  \fImarkedvarp(\fRvar);              |   \fI(markedvarp \fR'var)
  \fImarkvar(\fRvar);                 |   \fI(markvar \fR'var)
  \fIrecurunmark(\fRexp);             |   \fI(recurunmark \fR'exp)
  \fItempvar(\fRtype);                |   \fI(tempvar \fR'type)
  \fIunmarkvar(\fRvar);               |   \fI(unmarkvar \fR'var)
.ft
.DE
.DS L
.ft CR
Flags & Mode Switches:          | Flags & Mode Switches:
                                |
  \fIc\fR                             |   \fIc\fR
  \fIfloat\fR                         |   \fIfloat\fR
  \fIfortran\fR                       |   \fIfortran\fR
  \fIgendecs\fR                       |   \fIgendecs\fR
  \fIgentranopt\fR                    |   \fIgentranopt\fR
  \fIgentranparser\fR                 |   \fIgentranparser\fR
  \fIgentranseg\fR                    |   \fIgentranseg\fR
  \fIratfor\fR                        |   \fIratfor\fR
.ft
.DE
.DS L
.ft CR
Global Variables:               | Global Variables:
                                |
  \fI?ccurrind\e*\fR                   |   \fIccurrind*\fR
  \fI?clinelen\e*\fR                   |   \fIclinelen*\fR
  \fI?fortcurrind\e*\fR                |   \fIfortcurrind*\fR
  \fI?fortlinelen\e*\fR                |   \fIfortlinelen*\fR
  \fI?genstmtincr\e*\fR                |   \fIgenstmtincr*\fR
  \fI?genstmtno\e*\fR                  |   \fIgenstmtno*\fR
  \fI?gentranlang\e*\fR                |   \fIgentranlang*\fR
  \fI?gentranopt\e*\fR                 |   \fIgentranopt*\fR
  \fI?maxexpprintlen\e*\fR             |   \fImaxexpprintlen*\fR
  \fI?ratcurrind\e*\fR                 |   \fIratcurrind*\fR
  \fI?ratlinelen\e*\fR                 |   \fIratlinelen*\fR
  \fI?tablen\e*\fR                     |   \fItablen*\fR
  \fI?tempvarname\e*\fR                |   \fItempvarname*\fR
  \fI?tempvarnum\e*\fR                 |   \fItempvarnum*\fR
  \fI?tempvartype\e*\fR                |   \fItempvartype*\fR
.ft
.DE
