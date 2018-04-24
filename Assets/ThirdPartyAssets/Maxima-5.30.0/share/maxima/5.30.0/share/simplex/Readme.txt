SIMPLEX
========

The simplex package implements the two-phase standard simplex method for
solving linear programming problems in maxima.

The simplex package contains two files: simplex.lisp and
minimize_sx.mac. The file simplex.lisp should be compiled for speed. You
must load simplex before minimize_sx.


FUNCTIONS
==========

- linear_program(A, b, c): finds the vector x which minimizes c'.x subject
    to A.x=b. A is a n.m matrix, b a list of length n and c a list of length
    m. The return value is [x, val] where x is the minimizing vector and
    val=c'.x. This function is defined in the file simplex.lisp.

- minimize_sx(ex, const, pos): minimizes expression ex subject to
    constraints in the list const. pos is an optional arguments which lists
    non-negative variables. This function is defined in the file
    minimize_sx.

- maximize_sx(ex, const, pos): maximizes expression ex subject to
    constraints in the list const. pos is an optional arguments which lists
    non-negative variables. This function is defined in the file
    minimize_sx.


VARIABLES
==========

- epsilon_sx:     epsilon for numerical computation (float)
- scale_sx:       scale input (boolean)
- pivot_count_sx: the number of pivots in last computation (fixnum)
- pivot_max_sx:   maximum number of pivots allowed (fixnum)
- nonegative_sx:  assume all variables to minimize_sx/maximize_sx are
                  nonegative (boolean)

DEMO
=====

(%i1) A : matrix([1,1,-1,0],[2,-3,0,-1], [4,-5,0,0])$
(%i2) b : [1,1,6]$
(%i3) c : [1,-2,0,0]$
(%i4) linear_program(A, b, c);
(%o4) [[13/2, 4, 19/2, 0], -3/2]

(%i1) minimize_sx(x+y, [3*x+y>4, x+4*y>4]), nonegative_sx=true;
(%o1) [20/11, [x = 12/11, y = 8/11]]


TESTS
======

There are some tests in the Tests subdirectory.


AUTHOR
=======

This package was written by

  Andrej Vodopivec <andrejv@users.sourceforge.net>
  http://wxmaxima.sourceforge.net/maxima.html

It is licenced under the GPL licence.
