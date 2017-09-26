The core Maxima system (in src) should have consistent, good-quality,
and widely useful notations and functionality that are actively
maintained by us.  Every part of the core system should interoperate
nicely with every other part.  Functions should cover a large part of
their domain and give clean errors or noun forms when used outside
their domain. Notations should be unambiguous and consistent. All
Maxima programs can depend on the core functionality being available;
incompatible changes should be avoided.

The share directory contains additional functionality which may be
special-purpose or unmaintained.  The core system does not depend on
the share directory.  Although share packages should strive for the
consistency and quality of the core system, different packages may use
different and non-standard notations or conventions, which are not
necessarily recommended by the Maxima project.  Some share packages
may not only not interoperate correctly, but even cause problems if
loaded into the same Maxima system as other share packages.  Functions
may only cover special cases, and may even give incorrect results
silently if applied outside their design domain. Implementation
mechanisms may be much more inefficient than core algorithms
(e.g. using pattern matching for simplification). User documentation
for functions defined in share should be contained in the main Maxima
user documentation. Share code should have regression tests, but they
need not be a part of the standard Maxima test suite.

The contrib directory contains packages which may be suitable for the
core system or the share directories, but which have not been
evaluated fully enough to put there.

By these definitions, there are several parts of the core system which
probably belong in share.  One is poisson, which interoperates with
almost nothing else.  Another is cf, which uses list notation to
represent continued fractions -- no other part of the system
understands this.  Then there are cases where I think we agree that
the lack of interoperation is a bug we should fix (e.g. core matrix
inversion doesn't support floats or bfloats).


Stavros Macrakis