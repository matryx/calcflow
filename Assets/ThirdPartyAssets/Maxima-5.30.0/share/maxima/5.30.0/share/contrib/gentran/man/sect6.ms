.NH 1
EXAMPLES
.LP
Short examples have been given throughout this manual to
illustrate basic usage of the GENTRAN commands.  This section contains some
complete examples.
.NH 2
Example 1:  Interactive Code Generation
.LP
Suppose we wish to generate a FORTRAN subprogram which can be used
for computing the roots of a polynomial by Graeffe's Root-Squaring
Method[4].  This
.FS
[4] This is for instance convenient for ill-conditioned polynomials.  More
details are given in \fIIntroduction to Numerical Analysis\fR by
C. E. Froberg, Addison-Wesley Publishing Company, 1966.
.FE
method states that the roots
.RS
.DS L
.ft CR
x
 i
.ft
.DE
.RE
of a polynomial
.RS
.DS L
.ft CR
       ------
        \e         n-i
P (x) =  >    a  x
 n      /      i
       ------
.ft
.DE
.RE
can be found by constructing the polynomial
.RS
.DS L
.ft CR
     2          n      n-2       2       n-1      n-3       2
P* (x )  =  (a x  + a x    + ...)  - (a x    + a x    + ...)
  n           0      2                 1        3
.ft
.DE
.RE
with roots
.RS
.DS L
.ft CR
  2
x
 i
.ft
.DE
.RE
.LP
When \fIbatch\fR'd into VAXIMA, the following file of
VAXIMA statements will place the coefficients of
.RS
.DS L
.ft CR
P*
  n
.ft
.DE
.RE
into array b for some user-entered value of n greater than
zero:
.DS L
.ft CR
Contents of file graeffe.in:
+-----------------------------+
|q : 0$                       |
|for i:0 step 2 thru n do     |
|    q : q + a(i+1)*x^(n-i)$  |  [5]
|r : 0$                       |
|for i:1 step 2 thru n-1 do   |
|    r : r + a(i+1)*x^(n-i)$  |
|p : q^2 - r^2$               |
|p : ratsubst(y, x^2, p)$     |
|for i:0 thru n do            |
|    b[i] : ratcoeff(p, y, i)$|
+-----------------------------+
.ft
.DE
.FS
[5] In accordance with section 2.4, the subscripts of a are
i+1 instead of i.
.FE
Now a numerical subprogram can be generated with assignment statements
for the coefficients of
.RS
.DS L
.ft CR
P*
  n
.ft
.DE
.RE
(now stored in array b in VAXIMA).  Since these coefficients
are given in terms of the coefficients of
.RS
.DS L
.ft CR
P
 n
.ft
.DE
.RE
(i.e., subscripted variable a in VAXIMA), the subprogram will
need two parameters:  a and b, each of which must be arrays of
size n+1.
.LP
The following VAXIMA session will create subroutine graeff for
a polynomial of degree n=10 and write it to file graeffe.f:
.DS L
.ft CR
(c1) n : 10$
.ft
.DE
.DS L
.ft CR
(c2) batch("graeffe.in");

(c3) q : 0$

(c4) for i:0 step 2 thru n do
    q : q + a(i+1)*x^(n-i)$

(c5) r : 0$

(c6) for i:1 step 2 thru n-1 do
    r : r + a(i+1)*x^(n-i)$

(c7) p : q^2 - r^2$

(c8) p : ratsubst(y, x^2, p)$

(c9) for i:0 thru n do
    b[i] : ratcoeff(p, y, i)$

(d10)                      BATCH DONE
.ft
.DE
.DS L
.ft CR
(c11) gentranlang(fortran)$
.ft
.DE
.DS L
.ft CR
(c12) gentranpush("graeffe.f");

(d12)                      graeffe.f
.ft
.DE
.DS L
.ft CR
(c13) gentran( subroutine( graeff(a,b) ),
               type("real*8", a(eval(n+1)), b(eval(n+1))),
               literal("c",cr),
               literal("c",tab,"--  Graeffe Root-Squaring Method   --",cr),
               literal("c",tab,"--  to Find Roots of a Polynomial  --",cr),
               literal("c",cr) );

(d13)                      graeffe.f
.ft
.DE
.DS L
.ft CR
(c14) for i:1 thru n+1 do
          gentran( lrsetq( b[i], b[i-1] ) );

(d14)                      done
.ft
.DE
.DS L
.ft CR
(c15) gentran( return(), end() );

(d15)                      graeffe.f
.ft
.DE
.DS L
.ft CR
(c16) gentranpop(false);

(d16)                      true
.ft
.DE
.DS L
.ft CR
Contents of file graeffe.f:
+------------------------------------------------------------------------+
|      subroutine graeff(a,b)                                            |
|      real*8 a(11),b(11)                                                |
|c                                                                       |
|c     --  Graeffe Root-Squaring Method   --                             |
|c     --  to Find Roots of a Polynomial  --                             |
|c                                                                       |
|      b(1)=a(11)**2                                                     |
|      b(2)=-a(10)**2+2.0*a(9)*a(11)                                     |
|      b(3)=a(9)**2-2.0*a(8)*a(10)+2.0*a(7)*a(11)                        |
|      b(4)=-a(8)**2+2.0*a(7)*a(9)-2.0*a(6)*a(10)+2.0*a(5)*a(11)         |
|      b(5)=a(7)**2-2.0*a(6)*a(8)+2.0*a(5)*a(9)-2.0*a(4)*a(10)+2.0*a(3)*a|
|     . (11)                                                             |
|      b(6)=-a(6)**2+2.0*a(5)*a(7)-2.0*a(4)*a(8)+2.0*a(3)*a(9)-2.0*a(2)*a|
|     . (10)+2.0*a(1)*a(11)                                              |
|      b(7)=a(5)**2-2.0*a(4)*a(6)+2.0*a(3)*a(7)-2.0*a(2)*a(8)+2.0*a(1)*a(|
|     . 9)                                                               |
|      b(8)=-a(4)**2+2.0*a(3)*a(5)-2.0*a(2)*a(6)+2.0*a(1)*a(7)           |
|      b(9)=a(3)**2-2.0*a(2)*a(4)+2.0*a(1)*a(5)                          |
|      b(10)=-a(2)**2+2.0*a(1)*a(3)                                      |
|      b(11)=a(1)**2                                                     |
|      return                                                            |
|      end                                                               |
+------------------------------------------------------------------------+
.ft
.DE
.bp
.NH 2
Example 2:  Template Processing
.LP
Circuit simulation plays a vital role in computer hardware development.  A
recent paper[6] describes
.FS
[6] Loe, K. F., N. Ohsawa, and E. Goto.  "Design of an Automatic
Circuitry Code Generator (ACCG)", \fIRSYMSAC Proceedings\fR,
Riken, Wako-shi, Saitama, Japan.  August 21-22, 1984.
.FE
the design of an Automatic Circuitry Code Generator (ACCG),
which generates circuit simulation programs based on user-supplied
circuit specifications.  The actual code generator runs under the
REDUCE computer algebra system and consists of a series of
\fIwrite\fR statements, each of which writes one line of FORTRAN code.
.LP
This section presents an alternative implementation for the ACCG
in VAXIMA which uses GENTRAN's template processor to generate
code.  Template processing is a much more natural method of
code generation than the \fIwrite\fR statement method.
.LP
First we will put all VAXIMA calculations into two files:  rk.in
and ham.in.
.DS L
.ft CR
Contents of file rk.in:
+----------------------------------------------------------------+
|/*  Runge-Kutta Method  */                                      |
|                                                                |
|rungekutta(p1, p2, p, q, tt) :=                                 |
|  block( [k11, k12, k21, k22, k31, k32, k41, k42],              |
|         k11 : hh*p1,                                           |
|         k12 : hh*p2,                                           |
|         k21 : hh*ratsubst(q+k12/2, q,                          |
|                           ratsubst(p+k11/2, p,                 |
|                                    ratsubst(tt+hh/2, tt, p1))),|
|         k22 : hh*ratsubst(q+k12/2, q,                          |
|                           ratsubst(p+k11/2, p,                 |
|                                    ratsubst(tt+hh/2, tt, p2))),|
|         k31 : hh*ratsubst(q+k22/2, q,                          |
|                           ratsubst(p+k21/2, p,                 |
|                                    ratsubst(tt+hh/2, tt, p1))),|
|         k32 : hh*ratsubst(q+k22/2, q,                          |
|                           ratsubst(p+k21/2, p,                 |
|                                    ratsubst(tt+hh/2, tt, p2))),|
|         k41 : hh*ratsubst(q+k32, q,                            |
|                           ratsubst(p+k31, p,                   |
|                                    ratsubst(tt+hh, tt, p1))),  |
|         k42 : hh*ratsubst(q+k32, q,                            |  [7]
|                           ratsubst(p+k31, p,                   |
|                                    ratsubst(tt+hh, tt, p2))),  |
|         pn : ratsimp(p + (k11 + 2*k21 + 2*k31 + k41)/6),       |
|         qn : ratsimp(q + (k12 + 2*k22 + 2*k32 + k42)/6)        |
|       )$                                                       |
+----------------------------------------------------------------+
.ft
.DE
.FS
[7] Line 20 of procedure rungekutta was changed from an assignment
to k41, as given in [6], to an assignment to k42.
.FE
.DS L
.ft CR
Contents of file ham.in:
+--------------------------------------------------------+
|/*  Hamiltonian Calculation  */                         |
|                                                        |
|difq : diff(h, p)$                                      |
|difp : -diff(h, q) - ratsubst(p/m, qdot, diff(d, qdot))$|
|rungekutta(difp, difq, p, q, tt)$                       |
+--------------------------------------------------------+
.ft
.DE
.LP
Next we will create a template file with an outline of the target
FORTRAN program and GENTRAN commands.
.DS L
.ft CR
Contents of file runge.tem:
+---------------------------------------------------------------------+
|      program runge                                                  |
|      implicit real (k,m)                                            |
|c                                                                    |
|c  Input                                                             |
|c                                                                    |
|      write(6,*) 'Initial Value of p'                                |
|      read(5,*) p                                                    |
|      write(6,*) ' p = ', p                                          |
|      write(6,*) 'Initial Value of q'                                |
|      read(5,*) q                                                    |
|      write(6,*) ' q = ', q                                          |
|      write(6,*) 'Value of m'                                        |
|      read(5,*) m                                                    |
|      write(6,*) ' m = ', m                                          |
|      write(6,*) 'Value of k0'                                       |
|      read(5,*) k0                                                   |
|      write(6,*) ' k0 = ', k0                                        |
|      write(6,*) 'Value of b'                                        |
|      read(5,*) b                                                    |
|      write(6,*) ' b = ', b                                          |
|      write(6,*) 'Step Size of t'                                    |
|      read(5,*) hh                                                   |
|      write(6,*) ' Step Size of t = ', hh                            |
|      write(6,*) 'Final Value of t'                                  |
|      read(5,*) tp                                                   |
|      write(6,*) ' Final Value of t = ', tp                          |
.ft
.DE
.DS L
.ft CR
|c                                                                    |
|c  Initialization                                                    |
|c                                                                    |
|      tt=0.0                                                         |
|<<                                                                   |
|      gentran( literal(tab, "write(9,*) ' h = ", eval(h), "'", cr),  |
|               literal(tab, "write(9,*) ' d = ", eval(d), "'", cr) )$|
|>>                                                                   |
|      write(9,901) c                                                 |
|901   format(' c= ',e20.10)                                          |
|      write(9,910) tt, q, p                                          |
|910   format(' ',3e20.10)                                            |
|c                                                                    |
|c  Loop                                                              |
|c                                                                    |
|<<                                                                   |
|      gentran( unless tt >= tf do                                    |
|               (                                                     |
|                    rsetq(pn, ev(pn,expand)),                        |
|                    rsetq(q, ev(qn,expand)),                         |
|                    p : pn,                                          |
|                    tt : tt + hh,                                    |
|                    literal(tab, "write(9,910) tt, q, p", cr)        |
|               ) )$                                                  |
|>>                                                                   |
|      stop                                                           |
|      end                                                            |
+---------------------------------------------------------------------+
.ft
.DE
.LP
Now we can generate a circuit simulation program simply by
starting a VAXIMA session and following three steps:
.RS
.DS L
.ft CR
(1)  enter circuit specifications
(2)  perform calculations
(3)  call the GENTRAN template processor
.ft
.DE
.RE
For example, the following VAXIMA session will write a simulation
program to the file runge.f:
.DS L
.ft CR
/*  Enter Circuit Specifications  */

(c1) k : 1/(2*m)*p^2$
.ft
.DE
.DS L
.ft CR
(c2) u : k0/2*q^2$
.ft
.DE
.DS L
.ft CR
(c3) d : b/2*qdot$
.ft
.DE
.DS L
.ft CR
(c4) h : k + u$
.ft
.DE
.DS L
.ft CR
/*  Perform Calculations  */

(c5) batch("rk.in");

(c6) /*  Runge-Kutta Method  */

rungekutta(p1, p2, p, q, tt) :=
  block( [k11, k12, k21, k22, k31, k32, k41, k42],
         k11 : hh*p1,
         k12 : hh*p2,
         k21 : hh*ratsubst(q+k12/2, q,
                           ratsubst(p+k11/2, p,
                                    ratsubst(tt+hh/2, tt, p1))),
         k22 : hh*ratsubst(q+k12/2, q,
                           ratsubst(p+k11/2, p,
                                    ratsubst(tt+hh/2, tt, p2))),
         k31 : hh*ratsubst(q+k22/2, q,
                           ratsubst(p+k21/2, p,
                                    ratsubst(tt+hh/2, tt, p1))),
         k32 : hh*ratsubst(q+k22/2, q,
                           ratsubst(p+k21/2, p,
                                    ratsubst(tt+hh/2, tt, p2))),
         k41 : hh*ratsubst(q+k32, q,
                           ratsubst(p+k31, p,
                                    ratsubst(tt+hh, tt, p1))),
         k42 : hh*ratsubst(q+k32, q,
                           ratsubst(p+k31, p,
                                    ratsubst(tt+hh, tt, p2))),
         pn : ratsimp(p + (k11 + 2*k21 + 2*k31 + k41)/6),
         qn : ratsimp(q + (k12 + 2*k22 + 2*k32 + k42)/6)
       )$

(d7)                       BATCH DONE
.ft
.DE
.DS L
.ft CR
(c8) batch("ham.in");

(c9) /*  Hamiltonian Calculation  */

difq : diff(h, p)$

(c10) difp : -diff(h, q) - ratsubst(p/m, qdot, diff(d, qdot))$

(c11) rungekutta(difp, difq, p, q, tt)$

(d12)                      BATCH DONE
.ft
.DE
/*  Call Template Processor  */
.DS L
.ft CR
(c13) gentranlang(fortran)$
.ft
.DE
.DS L
.ft CR
(c14) on(gentranfloat)$
.ft
.DE
.DS L
.ft CR
(c15) gentranin("runge.tem", ["runge.f"]);

(d15)                      runge.f
.ft
.DE
.DS L
.ft CR
Contents of file runge.f:
+------------------------------------------------------------------------+
|      program runge                                                     |
|      implicit real (k,m)                                               |
|c                                                                       |
|c  Input                                                                |
|c                                                                       |
|      write(6,*) 'Initial Value of p'                                   |
|      read(5,*) p                                                       |
|      write(6,*) ' p = ', p                                             |
|      write(6,*) 'Initial Value of q'                                   |
|      read(5,*) q                                                       |
|      write(6,*) ' q = ', q                                             |
|      write(6,*) 'Value of m'                                           |
|      read(5,*) m                                                       |
|      write(6,*) ' m = ', m                                             |
|      write(6,*) 'Value of k0'                                          |
|      read(5,*) k0                                                      |
|      write(6,*) ' k0 = ', k0                                           |
|      write(6,*) 'Value of b'                                           |
|      read(5,*) b                                                       |
|      write(6,*) ' b = ', b                                             |
|      write(6,*) 'Step Size of t'                                       |
|      read(5,*) hh                                                      |
|      write(6,*) ' Step Size of t = ', hh                               |
|      write(6,*) 'Final Value of t'                                     |
|      read(5,*) tp                                                      |
|      write(6,*) ' Final Value of t = ', tp                             |
|c                                                                       |
|c  Initialization                                                       |
|c                                                                       |
|      tt=0.0                                                            |
|      write(9,*) ' h = p**2/2.0/m+k0*q**2/2.0'                          |
|      write(9,*) ' d = b*qdot/2.0'                                      |
|      write(9,901) c                                                    |
|901   format(' c= ',e20.10)                                             |
|      write(9,910) tt, q, p                                             |
|910   format(' ',3e20.10)                                               |
.ft
.DE
.DS L
.ft CR
|c                                                                       |
|c  Loop                                                                 |
|c                                                                       |
|25001 if (tt.ge.tf) goto 25002                                          |
|          pn=-1.0/2.0*b*hh-1.0/96.0*b*hh**5*k0**2/m**2+b*hh**3*k0/12.0/m|
|     .     +p-1.0/96.0*hh**6*k0**3*p/m**3+hh**4*k0**2*p/8.0/m**2-1.0/2.0|
|     .     *hh**2*k0*p/m-hh*k0*q-1.0/48.0*hh**5*k0**3*q/m**2+hh**3*k0**2|
|     .     *q/6.0/m                                                     |
|          q=b*hh**8*k0**3/384.0/m**4-1.0/48.0*b*hh**6*k0**2/m**3+b*hh**4|
|     .     *k0/12.0/m**2-1.0/4.0*b*hh**2/m+hh**9*k0**4*p/384.0/m**5-1.0/|
|     .     32.0*hh**7*k0**3*p/m**4+hh**5*k0**2*p/6.0/m**3-1.0/2.0*hh**3*|
|     .     k0*p/m**2+hh*p/m+q+hh**8*k0**4*q/192.0/m**4-1.0/24.0*hh**6*k0|
|     .     **3*q/m**3+hh**4*k0**2*q/6.0/m**2-1.0/2.0*hh**2*k0*q/m       |
|          p=pn                                                          |
|          tt=tt+hh                                                      |
|          write(9,910) tt, q, p                                         |
|          goto 25001                                                    |
|25002 continue                                                          |
|      stop                                                              |
|      end                                                               |
+------------------------------------------------------------------------+
.ft
.DE
.bp
.NH 2
Example 3:  Segmentation & Optimization Techniques
.LP
The following 3 x 3 inertia matrix, mat, was derived in the course
of research reported in [8]:
.FS
[8] Bos, A. M. and M. J. L. Tiernego.  "Formula Manipulation in the
Bond Graph Modelling and Simulation of Large Mechanical Systems",
\fIJournal of the Franklin Institute\fR, Pergamon Press Ltd.,
Vol. 319, No. 1/2, pp. 51-65, January/February 1985.
.FE
.DS L
.ft CR
mat[1,1] :
             2    2               2               2
    - 9 m30 p  sin (q3) + j30z sin (q3) - j30y sin (q3)
                2                           2        2
      + 18 m30 p  cos(q2) cos(q3) + 18 m30 p  + m10 p  + j30y

      + j10y
.ft
.DE
.DS L
.ft CR
mat[1,2] :
             2    2               2               2
    - 9 m30 p  sin (q3) + j30z sin (q3) - j30y sin (q3)
               2                          2
      + 9 m30 p  cos(q2) cos(q3) + 9 m30 p  + j30y
.ft
.DE
.DS L
.ft CR
mat[1,3] :
             2
    - 9 m30 p  sin(q2) sin(q3)
.ft
.DE
.DS L
.ft CR
mat[2,1] :
             2    2               2               2
    - 9 m30 p  sin (q3) + j30z sin (q3) - j30y sin (q3)
               2                          2
      + 9 m30 p  cos(q2) cos(q3) + 9 m30 p  + j30y
.ft
.DE
.DS L
.ft CR
mat[2,2] :
             2    2               2               2              2
    - 9 m30 p  sin (q3) + j30z sin (q3) - j30y sin (q3) + 9 m30 p

      + j30y
.ft
.DE
.DS L
.ft CR
mat[2,3] :

    0
.ft
.DE
.DS L
.ft CR
mat[3,1] :
            2
    - 9 m30 p  sin(q2) sin(q3)
.ft
.DE
.DS L
.ft CR
mat[3,2] :

    0
.ft
.DE
.DS L
.ft CR
mat[3,3] :
           2
    9 m30 p  + j30x
.ft
.DE
.LP
We know mat is symmetric.  We wish to generate numerical code to
compute values for mat and its inverse matrix, inv.
.NH 3
Code Generation
.LP
Generating code for matrix mat and its inverse matrix is
straightforward.  We can simply generate an assignment statement
for each element of mat, compute the inverse matrix, inv, and
generate an assignment statement for each element of inv.  Since
we know mat is symmetric across the main diagonal, we
know that inv will also be symmetric.  To avoid duplicate
computations, we will not generate assignments for elements
below the diagonals of these matrices.  Instead, we will generate
nested loops to copy elements across the main diagonals.
.LP
The following VAXIMA session will write to the file matrix.f:
.DS L
.ft CR
(c1) batch("matrix.v");

(c2) mat : genmatrix(mat, 3,3, 1,1)$

(c3) mat[1,1] :
     18*cos(q3)*cos(q2)*m30*p^2 - 9*sin(q3)^2*p^2*m30
     - sin(q3)^2*j30y + sin(q3)^2*j30z + p^2*m10 + 18*p^2*m30
     + j10y + j30y$

(c4) mat[2,1] :
     mat[1,2] :
     9*cos(q3)*cos(q2)*m30*p^2 - sin(q3)^2*j30y + sin(q3)^2*j30z
     - 9*sin(q3)^2*m30*p^2 + j30y + 9*m30*p^2$

(c5) mat[3,1] :
     mat[1,3] :
     -9*sin(q3)*sin(q2)*m30*p^2$
.ft
.DE
.DS L
.ft CR
(c6) mat[2,2] :
     -sin(q3)^2*j30y + sin(q3)^2*j30z - 9*sin(q3)^2 *m30*p^2
     + j30y + 9*m30*p^2$

(c7) mat[3,2] :
     mat[2,3] :
     0$

(c8) mat[3,3] :
     9*m30*p^2 + j30x$

(d9)                       BATCH DONE
.ft
.DE
.DS L
.ft CR
(c10) gentranout("matrix.f")$
.ft
.DE
.DS L
.ft CR
(c11) gentran( literal("c", cr,
                       "c --- Calculate Matrix Values ---", cr,
                       "c", cr) )$
.ft
.DE
.DS L
.ft CR
(c12) for i:1 thru 3 do
          for j:i thru 3 do
              gentran( lrsetq(mat[i,j], mat[i,j]) )$
.ft
.DE
.DS L
.ft CR
(c13) inv : mat^^(-1)$
.ft
.DE
.DS L
.ft CR
(c14) gentran( literal("c", cr,
                       "c --- Calculate Inverse Matrix Values ---",
                       cr, "c", cr) )$
.ft
.DE
.DS L
.ft CR
(c15) for i:1 thru 3 do
          for j:i thru 3 do
              gentran( lrsetq( inv[i,j], inv[i,j]) )$
.ft
.DE
.DS L
.ft CR
(c16) gentran( literal("c", cr,
                       "c --- Copy Entries Across Main Diagonals ---",
                       cr, "c", cr),
               for i:1 thru 3 do
                   for j:i+1 thru 3 do
                   (   mat[j,i] : mat[i,j],  inv[j,i] : inv[i,j]   )
             )$
.ft
.DE
.DS L
.ft CR
(c17) gentranshut("matrix.f")$
.ft
.DE
.DS L
.ft CR
Contents of file matrix.f:
+------------------------------------------------------------------------+
|c                                                                       |
|c --- Calculate Matrix Values ---                                       |
|c                                                                       |
|      mat(1,1)=j10y+j30y+m10*p**2+18.0*m30*p**2+18.0*m30*p**2*cos(q2)*  |
|     . cos(q3)-j30y*sin(q3)**2+j30z*sin(q3)**2-9.0*m30*p**2*sin(q3)**2  |
|      mat(1,2)=j30y+9.0*m30*p**2+9.0*m30*p**2*cos(q2)*cos(q3)-j30y*sin( |
|     . q3)**2+j30z*sin(q3)**2-9.0*m30*p**2*sin(q3)**2                   |
|      mat(1,3)=-9.0*m30*p**2*sin(q2)*sin(q3)                            |
|      mat(2,2)=j30y+9.0*m30*p**2-j30y*sin(q3)**2+j30z*sin(q3)**2-9.0*m30|
|     . *p**2*sin(q3)**2                                                 |
|      mat(2,3)=0.0                                                      |
|      mat(3,3)=j30x+9.0*m30*p**2                                        |
|c                                                                       |
|c --- Calculate Inverse Matrix Values ---                               |
|c                                                                       |
|      inv(1,1)=-(-j30x*j30y+(-9.0*j30x-9.0*j30y)*m30*p**2-81.0*m30**2*p |
|     . **4+(j30x*j30y-j30x*j30z+(9.0*j30x+9.0*j30y-9.0*j30z)*m30*p**2+  |
|     . 81.0*m30**2*p**4)*sin(q3)**2)/(j10y*j30x*j30y+(j30x*j30y*m10+(9.0|
|     . *j10y*j30x+(9.0*j10y+9.0*j30x)*j30y)*m30)*p**2+((9.0*j30x+9.0*   |
|     . j30y)*m10*m30+(81.0*j10y+81.0*j30x+81.0*j30y)*m30**2)*p**4+(81.0*|
|     . m10*m30**2+729.0*m30**3)*p**6+(-81.0*j30x*m30**2*p**4-729.0*m30**|
|     . 3*p**6)*cos(q2)**2*cos(q3)**2+(-j10y*j30x*j30y+j10y*j30x*j30z+((-|
|     . j30x*j30y+j30x*j30z)*m10+(-9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*   |
|     . j30y+(9.0*j10y+9.0*j30x)*j30z)*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0|
|     . *j30z)*m10*m30+(-81.0*j10y-81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)|
|     . *p**4+(-81.0*m10*m30**2-729.0*m30**3)*p**6+(-81.0*j30y*m30**2*p**|
|     . 4-729.0*m30**3*p**6)*sin(q2)**2)*sin(q3)**2+((81.0*j30y-81.0*j30z|
|     . )*m30**2*p**4+729.0*m30**3*p**6)*sin(q2)**2*sin(q3)**4)          |
|      inv(1,2)=(-j30x*j30y+(-9.0*j30x-9.0*j30y)*m30*p**2-81.0*m30**2*p**|
|     . 4+(-9.0*j30x*m30*p**2-81.0*m30**2*p**4)*cos(q2)*cos(q3)+(j30x*   |
|     . j30y-j30x*j30z+(9.0*j30x+9.0*j30y-9.0*j30z)*m30*p**2+81.0*m30**2*|
|     . p**4)*sin(q3)**2)/(j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+(|
|     . 9.0*j10y+9.0*j30x)*j30y)*m30)*p**2+((9.0*j30x+9.0*j30y)*m10*m30+(|
|     . 81.0*j10y+81.0*j30x+81.0*j30y)*m30**2)*p**4+(81.0*m10*m30**2+    |
|     . 729.0*m30**3)*p**6+(-81.0*j30x*m30**2*p**4-729.0*m30**3*p**6)*cos|
|     . (q2)**2*cos(q3)**2+(-j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+ |
|     . j30x*j30z)*m10+(-9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*j30y+(9.0*   |
|     . j10y+9.0*j30x)*j30z)*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0*j30z)*m10|
|     . *m30+(-81.0*j10y-81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)*p**4+(   |
|     . -81.0*m10*m30**2-729.0*m30**3)*p**6+(-81.0*j30y*m30**2*p**4-729.0|
|     . *m30**3*p**6)*sin(q2)**2)*sin(q3)**2+((81.0*j30y-81.0*j30z)*m30**|
|     . 2*p**4+729.0*m30**3*p**6)*sin(q2)**2*sin(q3)**4)                 |
|      inv(1,3)=-((-9.0*j30y*m30*p**2-81.0*m30**2*p**4)*sin(q2)*sin(q3)+(|
|     . (9.0*j30y-9.0*j30z)*m30*p**2+81.0*m30**2*p**4)*sin(q2)*sin(q3)**3|
|     . )/(j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+(9.0*j10y+9.0*   |
|     . j30x)*j30y)*m30)*p**2+((9.0*j30x+9.0*j30y)*m10*m30+(81.0*j10y+   |
|     . 81.0*j30x+81.0*j30y)*m30**2)*p**4+(81.0*m10*m30**2+729.0*m30**3)*|
|     . p**6+(-81.0*j30x*m30**2*p**4-729.0*m30**3*p**6)*cos(q2)**2*cos(q3|
|     . )**2+(-j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+j30x*j30z)*m10+|
|     . (-9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*j30y+(9.0*j10y+9.0*j30x)*   |
|     . j30z)*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*   |
|     . j10y-81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)*p**4+(-81.0*m10*m30**|
|     . 2-729.0*m30**3)*p**6+(-81.0*j30y*m30**2*p**4-729.0*m30**3*p**6)* |
|     . sin(q2)**2)*sin(q3)**2+((81.0*j30y-81.0*j30z)*m30**2*p**4+729.0* |
|     . m30**3*p**6)*sin(q2)**2*sin(q3)**4)                              |
|      inv(2,2)=-(-j10y*j30x-j30x*j30y+(-j30x*m10+(-9.0*j10y-18.0*j30x   |
|     . -9.0*j30y)*m30)*p**2+(-9.0*m10*m30-162.0*m30**2)*p**4+(-18.0*j30x|
|     . *m30*p**2-162.0*m30**2*p**4)*cos(q2)*cos(q3)+(j30x*j30y-j30x*j30z|
|     . +(9.0*j30x+9.0*j30y-9.0*j30z)*m30*p**2+81.0*m30**2*p**4+81.0*m30 |
|     . **2*p**4*sin(q2)**2)*sin(q3)**2)/(j10y*j30x*j30y+(j30x*j30y*m10+(|
|     . 9.0*j10y*j30x+(9.0*j10y+9.0*j30x)*j30y)*m30)*p**2+((9.0*j30x+9.0*|
|     . j30y)*m10*m30+(81.0*j10y+81.0*j30x+81.0*j30y)*m30**2)*p**4+(81.0*|
|     . m10*m30**2+729.0*m30**3)*p**6+(-81.0*j30x*m30**2*p**4-729.0*m30**|
|     . 3*p**6)*cos(q2)**2*cos(q3)**2+(-j10y*j30x*j30y+j10y*j30x*j30z+((-|
|     . j30x*j30y+j30x*j30z)*m10+(-9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*   |
|     . j30y+(9.0*j10y+9.0*j30x)*j30z)*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0|
|     . *j30z)*m10*m30+(-81.0*j10y-81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)|
|     . *p**4+(-81.0*m10*m30**2-729.0*m30**3)*p**6+(-81.0*j30y*m30**2*p**|
|     . 4-729.0*m30**3*p**6)*sin(q2)**2)*sin(q3)**2+((81.0*j30y-81.0*j30z|
|     . )*m30**2*p**4+729.0*m30**3*p**6)*sin(q2)**2*sin(q3)**4)          |
|      inv(2,3)=(((-9.0*j30y*m30*p**2-81.0*m30**2*p**4)*sin(q2)-81.0*m30 |
|     . **2*p**4*cos(q2)*sin(q2)*cos(q3))*sin(q3)+((9.0*j30y-9.0*j30z)*  |
|     . m30*p**2+81.0*m30**2*p**4)*sin(q2)*sin(q3)**3)/(j10y*j30x*j30y+( |
|     . j30x*j30y*m10+(9.0*j10y*j30x+(9.0*j10y+9.0*j30x)*j30y)*m30)*p**2+|
|     . ((9.0*j30x+9.0*j30y)*m10*m30+(81.0*j10y+81.0*j30x+81.0*j30y)*m30 |
|     . **2)*p**4+(81.0*m10*m30**2+729.0*m30**3)*p**6+(-81.0*j30x*m30**2*|
|     . p**4-729.0*m30**3*p**6)*cos(q2)**2*cos(q3)**2+(-j10y*j30x*j30y+  |
|     . j10y*j30x*j30z+((-j30x*j30y+j30x*j30z)*m10+(-9.0*j10y*j30x+(-9.0*|
|     . j10y-9.0*j30x)*j30y+(9.0*j10y+9.0*j30x)*j30z)*m30)*p**2+((-9.0*  |
|     . j30x-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y-81.0*j30x-81.0*j30y+ |
|     . 81.0*j30z)*m30**2)*p**4+(-81.0*m10*m30**2-729.0*m30**3)*p**6+(   |
|     . -81.0*j30y*m30**2*p**4-729.0*m30**3*p**6)*sin(q2)**2)*sin(q3)**2+|
|     . ((81.0*j30y-81.0*j30z)*m30**2*p**4+729.0*m30**3*p**6)*sin(q2)**2*|
|     . sin(q3)**4)                                                      |
|      inv(3,3)=-(-j10y*j30y+(-j30y*m10+(-9.0*j10y-9.0*j30y)*m30)*p**2+( |
|     . -9.0*m10*m30-81.0*m30**2)*p**4+81.0*m30**2*p**4*cos(q2)**2*cos(q3|
|     . )**2+(j10y*j30y-j10y*j30z+((j30y-j30z)*m10+(9.0*j10y+9.0*j30y-9.0|
|     . *j30z)*m30)*p**2+(9.0*m10*m30+81.0*m30**2)*p**4)*sin(q3)**2)/(   |
|     . j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+(9.0*j10y+9.0*j30x)*|
|     . j30y)*m30)*p**2+((9.0*j30x+9.0*j30y)*m10*m30+(81.0*j10y+81.0*j30x|
|     . +81.0*j30y)*m30**2)*p**4+(81.0*m10*m30**2+729.0*m30**3)*p**6+(   |
|     . -81.0*j30x*m30**2*p**4-729.0*m30**3*p**6)*cos(q2)**2*cos(q3)**2+(|
|     . -j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+j30x*j30z)*m10+(-9.0*|
|     . j10y*j30x+(-9.0*j10y-9.0*j30x)*j30y+(9.0*j10y+9.0*j30x)*j30z)*m30|
|     . )*p**2+((-9.0*j30x-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y-81.0*  |
|     . j30x-81.0*j30y+81.0*j30z)*m30**2)*p**4+(-81.0*m10*m30**2-729.0*  |
|     . m30**3)*p**6+(-81.0*j30y*m30**2*p**4-729.0*m30**3*p**6)*sin(q2)**|
|     . 2)*sin(q3)**2+((81.0*j30y-81.0*j30z)*m30**2*p**4+729.0*m30**3*p**|
|     . 6)*sin(q2)**2*sin(q3)**4)                                        |
|c                                                                       |
|c --- Copy Entries Across Main Diagonals ---                            |
|c                                                                       |
|      do 25001 i=1,3                                                    |
|          do 25002 j=i+1,3                                              |
|              mat(j,i)=mat(i,j)                                         |
|              inv(j,i)=inv(i,j)                                         |
|25002     continue                                                      |
|25001 continue                                                          |
+------------------------------------------------------------------------+
.ft
.DE
.NH 3
Segmentation
.LP
The assignment statements generated in the previous subsection
were several lines long.  Although none of the statements generated in
that example exceeded the 19 continuation line limit imposed by
most FORTRAN compilers, symbolic computations can easily yield expressions
which are much longer.  For this reason, the segmentation facility
automatically breaks long expressions into subexpressions of manageable
size.
.LP
For example, we could have generated segmented code by
setting the special variable \fI?maxexpprintlen\e*\fR to a smaller
value.  By preceding the VAXIMA session given in the previous
subsection by
.DS L
.ft CR
(c1) ?maxexpprintlen\e* : 300$
.ft
.DE
we would generate the following:
.DS L
.ft CR
Contents of file matrix.f:
+------------------------------------------------------------------------+
|c                                                                       |
|c --- Calculate Matrix Values ---                                       |
|c                                                                       |
|      mat(1,1)=j10y+j30y+m10*p**2+18.0*m30*p**2+18.0*m30*p**2*cos(q2)*  |
|     . cos(q3)-j30y*sin(q3)**2+j30z*sin(q3)**2-9.0*m30*p**2*sin(q3)**2  |
|      mat(1,2)=j30y+9.0*m30*p**2+9.0*m30*p**2*cos(q2)*cos(q3)-j30y*sin( |
|     . q3)**2+j30z*sin(q3)**2-9.0*m30*p**2*sin(q3)**2                   |
|      mat(1,3)=-9.0*m30*p**2*sin(q2)*sin(q3)                            |
|      mat(2,2)=j30y+9.0*m30*p**2-j30y*sin(q3)**2+j30z*sin(q3)**2-9.0*m30|
|     . *p**2*sin(q3)**2                                                 |
|      mat(2,3)=0.0                                                      |
|      mat(3,3)=j30x+9.0*m30*p**2                                        |
|c                                                                       |
|c --- Calculate Inverse Matrix Values ---                               |
|c                                                                       |
.ft
.DE
.DS L
.ft CR
|      t2=-(-j30x*j30y+(-9.0*j30x-9.0*j30y)*m30*p**2-81.0*m30**2*p**4+(  |
|     . j30x*j30y-j30x*j30z+(9.0*j30x+9.0*j30y-9.0*j30z)*m30*p**2+81.0*  |
|     . m30**2*p**4)*sin(q3)**2)                                         |
|      t0=j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+(9.0*j10y+9.0*j30x|
|     . )*j30y)*m30)*p**2+((9.0*j30x+9.0*j30y)*m10*m30+(81.0*j10y+81.0*  |
|     . j30x+81.0*j30y)*m30**2)*p**4+(81.0*m10*m30**2+729.0*m30**3)*p**6 |
|      t1=-j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+j30x*j30z)*m10+(   |
|     . -9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*j30y+(9.0*j10y+9.0*j30x)*j30z|
|     . )*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y   |
|     . -81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)*p**4                     |
|      inv(1,1)=t2/(t0+(-81.0*j30x*m30**2*p**4-729.0*m30**3*p**6)*cos(q2)|
|     . **2*cos(q3)**2+(t1+(-81.0*m10*m30**2-729.0*m30**3)*p**6+(-81.0*  |
|     . j30y*m30**2*p**4-729.0*m30**3*p**6)*sin(q2)**2)*sin(q3)**2+((81.0|
|     . *j30y-81.0*j30z)*m30**2*p**4+729.0*m30**3*p**6)*sin(q2)**2*sin(q3|
|     . )**4)                                                            |
|      t2=-j30x*j30y+(-9.0*j30x-9.0*j30y)*m30*p**2-81.0*m30**2*p**4+(-9.0|
|     . *j30x*m30*p**2-81.0*m30**2*p**4)*cos(q2)*cos(q3)+(j30x*j30y-j30x*|
|     . j30z+(9.0*j30x+9.0*j30y-9.0*j30z)*m30*p**2+81.0*m30**2*p**4)*sin(|
|     . q3)**2                                                           |
|      t0=j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+(9.0*j10y+9.0*j30x|
|     . )*j30y)*m30)*p**2+((9.0*j30x+9.0*j30y)*m10*m30+(81.0*j10y+81.0*  |
|     . j30x+81.0*j30y)*m30**2)*p**4+(81.0*m10*m30**2+729.0*m30**3)*p**6 |
|      t1=-j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+j30x*j30z)*m10+(   |
|     . -9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*j30y+(9.0*j10y+9.0*j30x)*j30z|
|     . )*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y   |
|     . -81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)*p**4                     |
|      inv(1,2)=t2/(t0+(-81.0*j30x*m30**2*p**4-729.0*m30**3*p**6)*cos(q2)|
|     . **2*cos(q3)**2+(t1+(-81.0*m10*m30**2-729.0*m30**3)*p**6+(-81.0*  |
|     . j30y*m30**2*p**4-729.0*m30**3*p**6)*sin(q2)**2)*sin(q3)**2+((81.0|
|     . *j30y-81.0*j30z)*m30**2*p**4+729.0*m30**3*p**6)*sin(q2)**2*sin(q3|
|     . )**4)                                                            |
|      t2=-((-9.0*j30y*m30*p**2-81.0*m30**2*p**4)*sin(q2)*sin(q3)+((9.0* |
|     . j30y-9.0*j30z)*m30*p**2+81.0*m30**2*p**4)*sin(q2)*sin(q3)**3)    |
|      t0=j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+(9.0*j10y+9.0*j30x|
|     . )*j30y)*m30)*p**2+((9.0*j30x+9.0*j30y)*m10*m30+(81.0*j10y+81.0*  |
|     . j30x+81.0*j30y)*m30**2)*p**4+(81.0*m10*m30**2+729.0*m30**3)*p**6 |
|      t1=-j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+j30x*j30z)*m10+(   |
|     . -9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*j30y+(9.0*j10y+9.0*j30x)*j30z|
|     . )*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y   |
|     . -81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)*p**4                     |
|      inv(1,3)=t2/(t0+(-81.0*j30x*m30**2*p**4-729.0*m30**3*p**6)*cos(q2)|
|     . **2*cos(q3)**2+(t1+(-81.0*m10*m30**2-729.0*m30**3)*p**6+(-81.0*  |
|     . j30y*m30**2*p**4-729.0*m30**3*p**6)*sin(q2)**2)*sin(q3)**2+((81.0|
|     . *j30y-81.0*j30z)*m30**2*p**4+729.0*m30**3*p**6)*sin(q2)**2*sin(q3|
|     . )**4)                                                            |
|      t0=-j10y*j30x-j30x*j30y+(-j30x*m10+(-9.0*j10y-18.0*j30x-9.0*j30y)*|
|     . m30)*p**2+(-9.0*m10*m30-162.0*m30**2)*p**4+(-18.0*j30x*m30*p**2  |
|     . -162.0*m30**2*p**4)*cos(q2)*cos(q3)                              |
|      t0=-(t0+(j30x*j30y-j30x*j30z+(9.0*j30x+9.0*j30y-9.0*j30z)*m30*p**2|
|     . +81.0*m30**2*p**4+81.0*m30**2*p**4*sin(q2)**2)*sin(q3)**2)       |
|      t1=j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+(9.0*j10y+9.0*j30x|
|     . )*j30y)*m30)*p**2+((9.0*j30x+9.0*j30y)*m10*m30+(81.0*j10y+81.0*  |
|     . j30x+81.0*j30y)*m30**2)*p**4+(81.0*m10*m30**2+729.0*m30**3)*p**6 |
|      t2=-j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+j30x*j30z)*m10+(   |
|     . -9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*j30y+(9.0*j10y+9.0*j30x)*j30z|
|     . )*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y   |
|     . -81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)*p**4                     |
|      inv(2,2)=t0/(t1+(-81.0*j30x*m30**2*p**4-729.0*m30**3*p**6)*cos(q2)|
|     . **2*cos(q3)**2+(t2+(-81.0*m10*m30**2-729.0*m30**3)*p**6+(-81.0*  |
|     . j30y*m30**2*p**4-729.0*m30**3*p**6)*sin(q2)**2)*sin(q3)**2+((81.0|
|     . *j30y-81.0*j30z)*m30**2*p**4+729.0*m30**3*p**6)*sin(q2)**2*sin(q3|
|     . )**4)                                                            |
|      t2=((-9.0*j30y*m30*p**2-81.0*m30**2*p**4)*sin(q2)-81.0*m30**2*p**4|
|     . *cos(q2)*sin(q2)*cos(q3))*sin(q3)+((9.0*j30y-9.0*j30z)*m30*p**2+ |
|     . 81.0*m30**2*p**4)*sin(q2)*sin(q3)**3                             |
|      t0=j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+(9.0*j10y+9.0*j30x|
|     . )*j30y)*m30)*p**2+((9.0*j30x+9.0*j30y)*m10*m30+(81.0*j10y+81.0*  |
|     . j30x+81.0*j30y)*m30**2)*p**4+(81.0*m10*m30**2+729.0*m30**3)*p**6 |
|      t1=-j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+j30x*j30z)*m10+(   |
|     . -9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*j30y+(9.0*j10y+9.0*j30x)*j30z|
|     . )*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y   |
|     . -81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)*p**4                     |
|      inv(2,3)=t2/(t0+(-81.0*j30x*m30**2*p**4-729.0*m30**3*p**6)*cos(q2)|
|     . **2*cos(q3)**2+(t1+(-81.0*m10*m30**2-729.0*m30**3)*p**6+(-81.0*  |
|     . j30y*m30**2*p**4-729.0*m30**3*p**6)*sin(q2)**2)*sin(q3)**2+((81.0|
|     . *j30y-81.0*j30z)*m30**2*p**4+729.0*m30**3*p**6)*sin(q2)**2*sin(q3|
|     . )**4)                                                            |
|      t0=-(-j10y*j30y+(-j30y*m10+(-9.0*j10y-9.0*j30y)*m30)*p**2+(-9.0*  |
|     . m10*m30-81.0*m30**2)*p**4+81.0*m30**2*p**4*cos(q2)**2*cos(q3)**2+|
|     . (j10y*j30y-j10y*j30z+((j30y-j30z)*m10+(9.0*j10y+9.0*j30y-9.0*j30z|
|     . )*m30)*p**2+(9.0*m10*m30+81.0*m30**2)*p**4)*sin(q3)**2)          |
|      t1=j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+(9.0*j10y+9.0*j30x|
|     . )*j30y)*m30)*p**2+((9.0*j30x+9.0*j30y)*m10*m30+(81.0*j10y+81.0*  |
|     . j30x+81.0*j30y)*m30**2)*p**4+(81.0*m10*m30**2+729.0*m30**3)*p**6 |
|      t2=-j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+j30x*j30z)*m10+(   |
|     . -9.0*j10y*j30x+(-9.0*j10y-9.0*j30x)*j30y+(9.0*j10y+9.0*j30x)*j30z|
|     . )*m30)*p**2+((-9.0*j30x-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y   |
|     . -81.0*j30x-81.0*j30y+81.0*j30z)*m30**2)*p**4                     |
|      inv(3,3)=t0/(t1+(-81.0*j30x*m30**2*p**4-729.0*m30**3*p**6)*cos(q2)|
|     . **2*cos(q3)**2+(t2+(-81.0*m10*m30**2-729.0*m30**3)*p**6+(-81.0*  |
|     . j30y*m30**2*p**4-729.0*m30**3*p**6)*sin(q2)**2)*sin(q3)**2+((81.0|
|     . *j30y-81.0*j30z)*m30**2*p**4+729.0*m30**3*p**6)*sin(q2)**2*sin(q3|
|     . )**4)                                                            |
|c                                                                       |
|c --- Copy Entries Across Main Diagonals ---                            |
|c                                                                       |
|      do 25001 i=1,3                                                    |
|          do 25002 j=i+1,3                                              |
|              mat(j,i)=mat(i,j)                                         |
|              inv(j,i)=inv(i,j)                                         |
|25002     continue                                                      |
|25001 continue                                                          |
+------------------------------------------------------------------------+
.ft
.DE
.NH 3
Expression Optimization
.LP
VAXIMA contains an \fIoptimize\fR command which takes an expression as
argument and returns a sequence of zero or more assignments followed
by another expression.  The returned expression will compute the same
value as the original expression after the assignments have been made,
but it will do so more efficiently by avoiding recomputation of some common
subexpressions.  This command can be applied to expressions generated
by GENTRAN by turning the \fIgentranopt\fR switch on.  Thus, if we issue
the command
.DS L
.ft CR
(c1) on(gentranopt)$
.ft
.DE
followed by the commands given in subsection 6.3.1, the following
will be generated:
.DS L
.ft CR
Contents of file matrix.f:
+------------------------------------------------------------------------+
|c                                                                       |
|c --- Calculate Matrix Values ---                                       |
|c                                                                       |
|      t0=p**2                                                           |
|      t1=sin(q3)**2                                                     |
|      mat(1,1)=j10y+j30y+m10*t0+18.0*m30*t0+18.0*m30*t0*cos(q2)*cos(q3)-|
|     . j30y*t1+j30z*t1-9.0*m30*t0*t1                                    |
|      t0=p**2                                                           |
|      t1=sin(q3)**2                                                     |
|      mat(1,2)=j30y+9.0*m30*t0+9.0*m30*t0*cos(q2)*cos(q3)-j30y*t1+j30z* |
|     . t1-9.0*m30*t0*t1                                                 |
|      mat(1,3)=-9.0*m30*p**2*sin(q2)*sin(q3)                            |
|      t0=p**2                                                           |
|      t1=sin(q3)**2                                                     |
|      mat(2,2)=j30y+9.0*m30*t0-j30y*t1+j30z*t1-9.0*m30*t0*t1            |
|      mat(2,3)=0.0                                                      |
|      mat(3,3)=j30x+9.0*m30*p**2                                        |
|c                                                                       |
|c --- Calculate Inverse Matrix Values ---                               |
|c                                                                       |
|      t0=-j30x*j30y                                                     |
|      t1=-9.0*j30x                                                      |
|      t2=-9.0*j30y                                                      |
|      t3=p**2                                                           |
|      t4=m30**2                                                         |
|      t5=p**4                                                           |
|      t6=9.0*j30x                                                       |
|      t7=9.0*j30y                                                       |
|      t8=sin(q3)                                                        |
|      t9=t8**2                                                          |
|      t10=9.0*j10y+t6                                                   |
|      t11=81.0*j30y                                                     |
|      t12=m30**3                                                        |
|      t13=p**6                                                          |
|      t14=-729.0*t12*t13                                                |
|      t15=sin(q2)**2                                                    |
.ft
.DE
.DS L
.ft CR
|      inv(1,1)=-(t0+(t1+t2)*m30*t3-81.0*t4*t5+(j30x*j30y-j30x*j30z+(t6+ |
|     . t7-9.0*j30z)*m30*t3+81.0*t4*t5)*t9)/(j10y*j30x*j30y+(j30x*j30y*  |
|     . m10+(9.0*j10y*j30x+t10*j30y)*m30)*t3+((t6+t7)*m10*m30+(81.0*j10y+|
|     . 81.0*j30x+t11)*t4)*t5+(81.0*m10*t4+729.0*t12)*t13+(-81.0*j30x*t4*|
|     . t5+t14)*cos(q2)**2*cos(q3)**2+(-j10y*j30x*j30y+j10y*j30x*j30z+(( |
|     . t0+j30x*j30z)*m10+(-9.0*j10y*j30x+(-9.0*j10y+t1)*j30y+t10*j30z)* |
|     . m30)*t3+((t1+t2+9.0*j30z)*m10*m30+(-81.0*j10y-81.0*j30x-81.0*j30y|
|     . +81.0*j30z)*t4)*t5+(-81.0*m10*t4-729.0*t12)*t13+(-81.0*j30y*t4*t5|
|     . +t14)*t15)*t9+((t11-81.0*j30z)*t4*t5+729.0*t12*t13)*t15*t8**4)   |
|      t0=-j30x*j30y                                                     |
|      t1=-9.0*j30x                                                      |
|      t2=-9.0*j30y                                                      |
|      t3=p**2                                                           |
|      t4=m30**2                                                         |
|      t5=p**4                                                           |
|      t6=-81.0*t4*t5                                                    |
|      t7=cos(q2)                                                        |
|      t8=cos(q3)                                                        |
|      t9=9.0*j30x                                                       |
|      t10=9.0*j30y                                                      |
|      t11=sin(q3)                                                       |
|      t12=t11**2                                                        |
|      t13=9.0*j10y+t9                                                   |
|      t14=81.0*j30y                                                     |
|      t15=m30**3                                                        |
|      t16=p**6                                                          |
|      t17=-729.0*t15*t16                                                |
|      t18=sin(q2)**2                                                    |
|      inv(1,2)=(t0+(t1+t2)*m30*t3+t6+(-9.0*j30x*m30*t3+t6)*t7*t8+(j30x* |
|     . j30y-j30x*j30z+(t9+t10-9.0*j30z)*m30*t3+81.0*t4*t5)*t12)/(j10y*  |
|     . j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+t13*j30y)*m30)*t3+((t9+  |
|     . t10)*m10*m30+(81.0*j10y+81.0*j30x+t14)*t4)*t5+(81.0*m10*t4+729.0*|
|     . t15)*t16+(-81.0*j30x*t4*t5+t17)*t7**2*t8**2+(-j10y*j30x*j30y+j10y|
|     . *j30x*j30z+((t0+j30x*j30z)*m10+(-9.0*j10y*j30x+(-9.0*j10y+t1)*   |
|     . j30y+t13*j30z)*m30)*t3+((t1+t2+9.0*j30z)*m10*m30+(-81.0*j10y-81.0|
|     . *j30x-81.0*j30y+81.0*j30z)*t4)*t5+(-81.0*m10*t4-729.0*t15)*t16+( |
|     . -81.0*j30y*t4*t5+t17)*t18)*t12+((t14-81.0*j30z)*t4*t5+729.0*t15* |
|     . t16)*t18*t11**4)                                                 |
|      t0=p**2                                                           |
|      t1=m30**2                                                         |
|      t2=p**4                                                           |
|      t3=sin(q2)                                                        |
|      t4=sin(q3)                                                        |
|      t5=9.0*j30y                                                       |
|      t6=9.0*j30x                                                       |
|      t7=9.0*j10y+t6                                                    |
|      t8=81.0*j30y                                                      |
|      t9=m30**3                                                         |
|      t10=p**6                                                          |
|      t11=-729.0*t9*t10                                                 |
|      t12=-9.0*j30x                                                     |
|      t13=t3**2                                                         |
|      inv(1,3)=-((-9.0*j30y*m30*t0-81.0*t1*t2)*t3*t4+((t5-9.0*j30z)*m30*|
|     . t0+81.0*t1*t2)*t3*t4**3)/(j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y|
|     . *j30x+t7*j30y)*m30)*t0+((t6+t5)*m10*m30+(81.0*j10y+81.0*j30x+t8)*|
|     . t1)*t2+(81.0*m10*t1+729.0*t9)*t10+(-81.0*j30x*t1*t2+t11)*cos(q2) |
|     . **2*cos(q3)**2+(-j10y*j30x*j30y+j10y*j30x*j30z+((-j30x*j30y+j30x*|
|     . j30z)*m10+(-9.0*j10y*j30x+(-9.0*j10y+t12)*j30y+t7*j30z)*m30)*t0+(|
|     . (t12-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y-81.0*j30x-81.0*j30y+ |
|     . 81.0*j30z)*t1)*t2+(-81.0*m10*t1-729.0*t9)*t10+(-81.0*j30y*t1*t2+ |
|     . t11)*t13)*t4**2+((t8-81.0*j30z)*t1*t2+729.0*t9*t10)*t13*t4**4)   |
|      t0=-j30x*j30y                                                     |
|      t1=-9.0*j10y                                                      |
|      t2=-9.0*j30y                                                      |
|      t3=p**2                                                           |
|      t4=m30**2                                                         |
|      t5=p**4                                                           |
|      t6=cos(q2)                                                        |
|      t7=cos(q3)                                                        |
|      t8=9.0*j30x                                                       |
|      t9=9.0*j30y                                                       |
|      t10=sin(q2)**2                                                    |
|      t11=sin(q3)                                                       |
|      t12=t11**2                                                        |
|      t13=9.0*j10y+t8                                                   |
|      t14=81.0*j30y                                                     |
|      t15=m30**3                                                        |
|      t16=p**6                                                          |
|      t17=-729.0*t15*t16                                                |
|      t18=-9.0*j30x                                                     |
|      inv(2,2)=-(-j10y*j30x+t0+(-j30x*m10+(t1-18.0*j30x+t2)*m30)*t3+(   |
|     . -9.0*m10*m30-162.0*t4)*t5+(-18.0*j30x*m30*t3-162.0*t4*t5)*t6*t7+(|
|     . j30x*j30y-j30x*j30z+(t8+t9-9.0*j30z)*m30*t3+81.0*t4*t5+81.0*t4*t5|
|     . *t10)*t12)/(j10y*j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+t13*j30y|
|     . )*m30)*t3+((t8+t9)*m10*m30+(81.0*j10y+81.0*j30x+t14)*t4)*t5+(81.0|
|     . *m10*t4+729.0*t15)*t16+(-81.0*j30x*t4*t5+t17)*t6**2*t7**2+(-j10y*|
|     . j30x*j30y+j10y*j30x*j30z+((t0+j30x*j30z)*m10+(-9.0*j10y*j30x+(t1+|
|     . t18)*j30y+t13*j30z)*m30)*t3+((t18+t2+9.0*j30z)*m10*m30+(-81.0*   |
|     . j10y-81.0*j30x-81.0*j30y+81.0*j30z)*t4)*t5+(-81.0*m10*t4-729.0*  |
|     . t15)*t16+(-81.0*j30y*t4*t5+t17)*t10)*t12+((t14-81.0*j30z)*t4*t5+ |
|     . 729.0*t15*t16)*t10*t11**4)                                       |
|      t0=p**2                                                           |
|      t1=m30**2                                                         |
|      t2=p**4                                                           |
|      t3=sin(q2)                                                        |
|      t4=cos(q2)                                                        |
|      t5=cos(q3)                                                        |
|      t6=sin(q3)                                                        |
|      t7=9.0*j30y                                                       |
|      t8=9.0*j30x                                                       |
|      t9=9.0*j10y+t8                                                    |
|      t10=81.0*j30y                                                     |
|      t11=m30**3                                                        |
|      t12=p**6                                                          |
|      t13=-729.0*t11*t12                                                |
|      t14=-9.0*j30x                                                     |
|      t15=t3**2                                                         |
|      inv(2,3)=(((-9.0*j30y*m30*t0-81.0*t1*t2)*t3-81.0*t1*t2*t4*t3*t5)* |
|     . t6+((t7-9.0*j30z)*m30*t0+81.0*t1*t2)*t3*t6**3)/(j10y*j30x*j30y+( |
|     . j30x*j30y*m10+(9.0*j10y*j30x+t9*j30y)*m30)*t0+((t8+t7)*m10*m30+( |
|     . 81.0*j10y+81.0*j30x+t10)*t1)*t2+(81.0*m10*t1+729.0*t11)*t12+(    |
|     . -81.0*j30x*t1*t2+t13)*t4**2*t5**2+(-j10y*j30x*j30y+j10y*j30x*j30z|
|     . +((-j30x*j30y+j30x*j30z)*m10+(-9.0*j10y*j30x+(-9.0*j10y+t14)*j30y|
|     . +t9*j30z)*m30)*t0+((t14-9.0*j30y+9.0*j30z)*m10*m30+(-81.0*j10y   |
|     . -81.0*j30x-81.0*j30y+81.0*j30z)*t1)*t2+(-81.0*m10*t1-729.0*t11)* |
|     . t12+(-81.0*j30y*t1*t2+t13)*t15)*t6**2+((t10-81.0*j30z)*t1*t2+    |
|     . 729.0*t11*t12)*t15*t6**4)                                        |
|      t0=-9.0*j10y                                                      |
|      t1=-9.0*j30y                                                      |
|      t2=p**2                                                           |
|      t3=m30**2                                                         |
|      t4=p**4                                                           |
|      t5=cos(q2)**2                                                     |
|      t6=cos(q3)**2                                                     |
|      t7=9.0*j10y                                                       |
|      t8=9.0*j30y                                                       |
|      t9=sin(q3)                                                        |
|      t10=t9**2                                                         |
|      t11=9.0*j30x                                                      |
|      t12=t7+t11                                                        |
|      t13=81.0*j30y                                                     |
|      t14=m30**3                                                        |
|      t15=p**6                                                          |
|      t16=-729.0*t14*t15                                                |
|      t17=-9.0*j30x                                                     |
|      t18=sin(q2)**2                                                    |
|      inv(3,3)=-(-j10y*j30y+(-j30y*m10+(t0+t1)*m30)*t2+(-9.0*m10*m30    |
|     . -81.0*t3)*t4+81.0*t3*t4*t5*t6+(j10y*j30y-j10y*j30z+((j30y-j30z)* |
|     . m10+(t7+t8-9.0*j30z)*m30)*t2+(9.0*m10*m30+81.0*t3)*t4)*t10)/(j10y|
|     . *j30x*j30y+(j30x*j30y*m10+(9.0*j10y*j30x+t12*j30y)*m30)*t2+((t11+|
|     . t8)*m10*m30+(81.0*j10y+81.0*j30x+t13)*t3)*t4+(81.0*m10*t3+729.0* |
|     . t14)*t15+(-81.0*j30x*t3*t4+t16)*t5*t6+(-j10y*j30x*j30y+j10y*j30x*|
|     . j30z+((-j30x*j30y+j30x*j30z)*m10+(-9.0*j10y*j30x+(t0+t17)*j30y+  |
|     . t12*j30z)*m30)*t2+((t17+t1+9.0*j30z)*m10*m30+(-81.0*j10y-81.0*   |
|     . j30x-81.0*j30y+81.0*j30z)*t3)*t4+(-81.0*m10*t3-729.0*t14)*t15+(  |
|     . -81.0*j30y*t3*t4+t16)*t18)*t10+((t13-81.0*j30z)*t3*t4+729.0*t14* |
|     . t15)*t18*t9**4)                                                  |
|c                                                                       |
|c --- Copy Entries Across Main Diagonals ---                            |
|c                                                                       |
|      do 25001 i=1,3                                                    |
|          do 25002 j=i+1,3                                              |
|              mat(j,i)=mat(i,j)                                         |
|              inv(j,i)=inv(i,j)                                         |
|25002     continue                                                      |
|25001 continue                                                          |
+------------------------------------------------------------------------+
.ft
.DE
.LP
VAXIMA's optimize facility is limited in that it can only optimize
one expression at a time.  Therefore, as can be seen in this example,
subexpressions which are common to two or more assignments are needlessly
regenerated.
.NH 3
Generation of Temporary Variables to Suppress Simplification
.LP
We can dramatically improve upon the efficiency of the code generated
in the previous subsections by replacing expressions by temporary
variables \fIbefore\fR computing the inverse matrix.  This effectively
suppresses simplification; these expressions will not be substituted
into later computations.  We will replace each non-zero element
of VAXIMA matrix mat by a generated variable name and generate
a FORTRAN assignment statement to reflect that
substitution in the numerical program being generated.
.LP
The following VAXIMA session will write generated code to file
matrix.f:
.DS L
.ft CR
(c1) batch("matrix.v");

(c2) mat : genmatrix(mat, 3,3, 1,1)$

(c3) mat[1,1] :
     18*cos(q3)*cos(q2)*m30*p^2 - 9*sin(q3)^2*p^2*m30
     - sin(q3)^2*j30y + sin(q3)^2*j30z + p^2*m10 + 18*p^2*m30
     + j10y + j30y$

(c4) mat[2,1] :
     mat[1,2] :
     9*cos(q3)*cos(q2)*m30*p^2 - sin(q3)^2*j30y + sin(q3)^2*j30z
     - 9*sin(q3)^2*m30*p^2 + j30y + 9*m30*p^2$

(c5) mat[3,1] :
     mat[1,3] :
     -9*sin(q3)*sin(q2)*m30*p^2$

(c6) mat[2,2] :
     -sin(q3)^2*j30y + sin(q3)^2*j30z - 9*sin(q3)^2 *m30*p^2
     + j30y + 9*m30*p^2$

(c7) mat[3,2] :
     mat[2,3] :
     0$

(c8) mat[3,3] :
     9*m30*p^2 + j30x$

(d9)                       BATCH DONE
.ft
.DE
.DS L
.ft CR
(c10) gentranout("matrix.f")$
.ft
.DE
.DS L
.ft CR
(c11) gentran( literal("c", cr,
                       "c --- Calculate Matrix Values ---", cr,
                       "c", cr) )$
.ft
.DE
.DS L
.ft CR
(c12) for i:1 thru 3 do
          for j:i thru 3 do
              gentran( lrsetq(mat[i,j], mat[i,j]) )$
.ft
.DE
.DS L
.ft CR
(c13) gentran( literal("c", cr,
                       "c --- Assign Non-Zero Matrix Values to ",
                       "Temporary Variables ---", cr, "c", cr) )$
.ft
.DE
.DS L
.ft CR
(c14) for i:1 thru 3 do
          for j:i thru 3 do
              if mat[i,j]#0 then
              (
                  markvar( var : tempvar(false) ),
                  mat[i,j] : mat[j,i] : var,
                  gentran( eval(var) : mat[eval(i),eval(j)] )
              )$
.ft
.DE
.DS L
.ft CR
(c15) mat;        /***  Contents of Matrix mat:  ***/

                           [ t0   t1   t2 ]
                           [              ]
(d15)                      [ t1   t3    0 ]
                           [              ]
                           [ t2    0   t4 ]
.ft
.DE
.DS L
.ft CR
(c16) inv : mat^^(-1)$
.ft
.DE
.DS L
.ft CR
(c17) gentran( literal("c", cr,
                       "c --- Calculate Inverse Matrix Values ---",
                       cr, "c", cr) )$
.ft
.DE
.DS L
.ft CR
(c18) for i:1 thru 3 do
          for j:i thru 3 do
              gentran( lrsetq(inv[i,j], inv[i,j]) )$
.ft
.DE
.DS L
.ft CR
(c19) gentran( literal("c", cr,
                       "c --- Copy Entries Across Main Diagonals ---",
                       cr, "c", cr),
               for i:1 thru 3 do
                   for j:i+1 thru 3 do
                   (   mat[j,i] : mat[i,j],  inv[j,i] : inv[i,j]   )
             )$
.ft
.DE
.DS L
.ft CR
(c20) gentranshut("matrix.f")$
.ft
.DE
.DS L
.ft CR
Contents of file matrix.f:
+------------------------------------------------------------------------+
|c                                                                       |
|c --- Calculate Matrix Values ---                                       |
|c                                                                       |
|      mat(1,1)=j10y+j30y+m10*p**2+18.0*m30*p**2+18.0*m30*p**2*cos(q2)*  |
|     . cos(q3)-j30y*sin(q3)**2+j30z*sin(q3)**2-9.0*m30*p**2*sin(q3)**2  |
|      mat(1,2)=j30y+9.0*m30*p**2+9.0*m30*p**2*cos(q2)*cos(q3)-j30y*sin( |
|     . q3)**2+j30z*sin(q3)**2-9.0*m30*p**2*sin(q3)**2                   |
|      mat(1,3)=-9.0*m30*p**2*sin(q2)*sin(q3)                            |
|      mat(2,2)=j30y+9.0*m30*p**2-j30y*sin(q3)**2+j30z*sin(q3)**2-9.0*m30|
|     . *p**2*sin(q3)**2                                                 |
|      mat(2,3)=0.0                                                      |
|      mat(3,3)=j30x+9.0*m30*p**2                                        |
|c                                                                       |
|c --- Assign Non-Zero Matrix Values to Temporary Variables ---          |
|c                                                                       |
|      t0=mat(1,1)                                                       |
|      t1=mat(1,2)                                                       |
|      t2=mat(1,3)                                                       |
|      t3=mat(2,2)                                                       |
|      t4=mat(3,3)                                                       |
|c                                                                       |
|c --- Calculate Inverse Matrix Values ---                               |
|c                                                                       |
|      inv(1,1)=t3*t4/(-t2**2*t3+(-t1**2+t0*t3)*t4)                      |
|      inv(1,2)=-t1*t4/(-t2**2*t3+(-t1**2+t0*t3)*t4)                     |
|      inv(1,3)=-t2*t3/(-t2**2*t3+(-t1**2+t0*t3)*t4)                     |
|      inv(2,2)=(-t2**2+t0*t4)/(-t2**2*t3+(-t1**2+t0*t3)*t4)             |
|      inv(2,3)=t1*t2/(-t2**2*t3+(-t1**2+t0*t3)*t4)                      |
|      inv(3,3)=(-t1**2+t0*t3)/(-t2**2*t3+(-t1**2+t0*t3)*t4)             |
|c                                                                       |
|c --- Copy Entries Across Main Diagonals ---                            |
|c                                                                       |
|      do 25001 i=1,3                                                    |
|          do 25002 j=i+1,3                                              |
|              mat(j,i)=mat(i,j)                                         |
|              inv(j,i)=inv(i,j)                                         |
|25002     continue                                                      |
|25001 continue                                                          |
+------------------------------------------------------------------------+
.ft
.DE
.NH 3
Expression Optimization & Generation of Temporary Variables
.LP
By combining expression optimization (section 6.3.3) with generation
of temporary variables (section 6.3.4), we can generate code which
is even more efficient than any generated thus far in this
section.
.LP
By turning on the \fIgentranopt\fR flag before issuing the VAXIMA
commands given in the previous subsection, the following code will be
generated:
.DS L
.ft CR
Contents of file matrix.f:
+------------------------------------------------------------------------+
|c                                                                       |
|c --- Calculate Matrix Values ---                                       |
|c                                                                       |
|      t0=p**2                                                           |
|      t1=sin(q3)**2                                                     |
|      mat(1,1)=j10y+j30y+m10*t0+18.0*m30*t0+18.0*m30*t0*cos(q2)*cos(q3)-|
|     . j30y*t1+j30z*t1-9.0*m30*t0*t1                                    |
|      t0=p**2                                                           |
|      t1=sin(q3)**2                                                     |
|      mat(1,2)=j30y+9.0*m30*t0+9.0*m30*t0*cos(q2)*cos(q3)-j30y*t1+j30z* |
|     . t1-9.0*m30*t0*t1                                                 |
|      mat(1,3)=-9.0*m30*p**2*sin(q2)*sin(q3)                            |
|      t0=p**2                                                           |
|      t1=sin(q3)**2                                                     |
|      mat(2,2)=j30y+9.0*m30*t0-j30y*t1+j30z*t1-9.0*m30*t0*t1            |
|      mat(2,3)=0.0                                                      |
|      mat(3,3)=j30x+9.0*m30*p**2                                        |
|c                                                                       |
|c --- Assign Non-Zero Matrix Values to Temporary Variables ---          |
|c                                                                       |
|      t0=mat(1,1)                                                       |
|      t1=mat(1,2)                                                       |
|      t2=mat(1,3)                                                       |
|      t3=mat(2,2)                                                       |
|      t4=mat(3,3)                                                       |
|c                                                                       |
|c --- Calculate Inverse Matrix Values ---                               |
|c                                                                       |
|      inv(1,1)=t3*t4/(-t2**2*t3+(-t1**2+t0*t3)*t4)                      |
|      inv(1,2)=-t1*t4/(-t2**2*t3+(-t1**2+t0*t3)*t4)                     |
|      inv(1,3)=-t2*t3/(-t2**2*t3+(-t1**2+t0*t3)*t4)                     |
|      t5=t2**2                                                          |
|      inv(2,2)=(-t5+t0*t4)/(-t5*t3+(-t1**2+t0*t3)*t4)                   |
|      inv(2,3)=t1*t2/(-t2**2*t3+(-t1**2+t0*t3)*t4)                      |
|      t5=-t1**2+t0*t3                                                   |
|      inv(3,3)=t5/(-t2**2*t3+t5*t4)                                     |
.ft
.DE
.DS L
.ft CR
|c                                                                       |
|c --- Copy Entries Across Main Diagonals ---                            |
|c                                                                       |
|      do 25001 i=1,3                                                    |
|          do 25002 j=i+1,3                                              |
|              mat(j,i)=mat(i,j)                                         |
|              inv(j,i)=inv(i,j)                                         |
|25002     continue                                                      |
|25001 continue                                                          |
+------------------------------------------------------------------------+
.ft
.DE
.NH 3
Comparison of Code Optimization Techniques
.LP
As previously mentioned, the VAXIMA \fIoptimize\fR facility can only
optimize one expression at a time, and thus, subexpressions common
to two or more assignment statements are needlessly regenerated.  A
code optimizer /11/ designed by J. A. van Hulzen, which runs under
REDUCE, is much more powerful.  If we generate code with
GENTRAN under the REDUCE system in much the same way
as we did in the previous subsection under VAXIMA, but call van Hulzen's code
optimizer to optimize the assignments, the following code will be
generated:
.DS L
.ft CR
Contents of file matrix.f:
+--------------------------------------------------------------+
|c                                                             |
|c --- Calculate Matrix Values ---                             |
|c                                                             |
|      t0=cos(q3)                                              |
|      t1=cos(q2)                                              |
|      t2=sin(q3)                                              |
|      t8=t2*t2                                                |
|      t7=p*p                                                  |
|      t5=t7*m30                                               |
|      t4=t5*t0*t1                                             |
|      t6=-j30y+j30z                                           |
|      t14=9.0*t5                                              |
|      t10=-t14+t6                                             |
|      t11=t10*t8                                              |
|      t13=t5+t4                                               |
|      mat(1,1)=18.0*t13+t11+j30y+j10y+t7*m10                  |
|      t9=t14+j30y                                             |
|      mat(2,2)=t11+t9                                         |
.ft
.DE
.DS L
.ft CR
|      mat(1,2)=mat(2,2)+9.0*t4                                |
|      t3=sin(q2)                                              |
|      mat(1,3)=-t14*t2*t3                                     |
|      mat(2,3)=0.0                                            |
|      mat(3,3)=t14+j30x                                       |
|c                                                             |
|c --- Assign Non-Zero Matrix Values to Temporary Variables ---|
|c                                                             |
|      t0=mat(1,1)                                             |
|      t1=mat(1,2)                                             |
|      t2=mat(1,3)                                             |
|      t3=mat(2,2)                                             |
|      t4=mat(3,3)                                             |
|c                                                             |
|c --- Calculate Inverse Matrix Values ---                     |
|c                                                             |
|      t5=t3*t4                                                |
|      t6=t4*t1                                                |
|      t12=t6*t1                                               |
|      t13=t4*t0                                               |
|      t14=t2*t2                                               |
|      t8=-t14+t13                                             |
|      t15=t8*t3                                               |
|      t11=t15-t12                                             |
|      inv(1,1)=t5/t11                                         |
|      inv(1,2)=t6/t11                                         |
|      t7=-t3*t2                                               |
|      inv(1,3)=t7/t11                                         |
|      inv(2,2)=t8/t11                                         |
|      t9=t2*t1                                                |
|      inv(2,3)=t9/t11                                         |
|      t16=t1*t1                                               |
|      t10=-t16+t3*t0                                          |
|      inv(3,3)=t10/t11                                        |
|c                                                             |
|c --- Copy Entries Across Main Diagonals ---                  |
|c                                                             |
|      do 25001 i=1,3                                          |
|         do 25002 j=i+1,3                                     |
|             mat(j,i)=mat(i,j)                                |
|             inv(j,i)=inv(i,j)                                |
|25002     continue                                            |
|25001 continue                                                |
+--------------------------------------------------------------+
.ft
.DE
.LP
Not only was van Hulzen's Code Optimizer able to detect and
consistently replace subexpressions common to more than one expression, it
also improved the efficiency of the resultant code by replacing all
exponentiations by optimal sequences of products.
.LP
The following table compares the efficiency of code generated
in each of the previous subsections:
.DS L
.ft CR
                                    | +,- | *,/ |  ** | sin,cos |
 -----------------------------------+-----+-----+-----+---------+
 6.3.1. code generation             | 369 | 645 | 257 |      73 |
 6.3.2. segmentation                | 369 | 645 | 257 |      73 |
 6.3.3. optimization (VAXIMA)       | 368 | 580 |  70 |      33 |
 6.3.4. temporary variables         |  49 |  66 |  34 |      15 |
 6.3.5. temp. vars. & opt. (VAXIMA) |  47 |  65 |  20 |       9 |
 6.3.6. temp. vars. & opt. (REDUCE) |  21 |  28 |   0 |       4 |
 -----------------------------------+-----+-----+-----+---------+
.ft
.DE
