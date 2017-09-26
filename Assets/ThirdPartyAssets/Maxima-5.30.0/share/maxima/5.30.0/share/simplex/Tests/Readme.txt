TESTS
======

This directory contains tests for the Simplex package.

klee_minty
===========

The function klee_minty produces input for LinearProgram, for which
exponential time for solving is required without scaling.

Example:
   load(klee_minty)$
   apply(linear_program, klee_minty(6));
 A better approach:
   epsilon_sx : 0$
   scale_sx : true$
   apply(linear_program, klee_minty(10));

NETLIB
=======

Some smaller problems from netlib (http://www.netlib.org/lp/data/) test
suite are converted to a format, readable by Maxima. Problems are adlittle,
afiro, kb2 and sc50a. Each problem has three input files in CSV format for
matrix A and vectors b and c.

Example:
   load("numericalio/numericalio");
   A : read_matrix("adlittle_A.csv", 'csv)$
   b : read_list("adlittle_b.csv", 'csv)$
   c : read_list("adlittle_c.csv", 'csv)$
   linear_program(A, b, c)$
   %[2]
   => 225494.963126615

Results:
  PROBLEM        MINIMUM                SCALING
  adlittle       225494.963126615       no
  afiro          - 464.7531428571429    no
  kb2            - 1749.900129055996    yes
  sc50a          - 64.5750770585645     no
