.DS L
.ft CR
                            \fBCONTENTS\fR



1.  INTRODUCTION  ...........................................   4

    1.1.  The GENTRAN Code Generator and Translator
    1.2.  Organization of the Manual
          1.2.1.  Typographic Conventions

2.  CODE GENERATION  ........................................   6

    2.1.  Target Language Selection
    2.2.  Translation
          2.2.1.  Special Arguments
    2.3.  Code Generation:  Evaluation Prior to Translation
          2.3.1.  The \fBeval\fR Function
          2.3.2.  The \fBrsetq\fR Function
          2.3.3.  The \fBlsetq\fR Function
          2.3.4.  The \fBlrsetq\fR Function
    2.4.  Type Declarations
          2.4.1.  Subprogram Type Declarations
          2.4.2.  A Note on Subscripted Variables
    2.5.  Comments and Literal Strings
    2.6.  The \fBfortran\fR, \fBratfor\fR, and \fBc\fR Mode Switches
    2.7.  Code Optimization
    2.8.  Expression Segmentation
          2.8.1.  Implicit Type Declarations
          2.8.2.  Controlling Generation of Declarations
    2.9.  Generation of Temporary Variable Names
          2.9.1.  Marking Temporary Variables
          2.9.2.  Unmarking Temporary Variables
          2.9.3.  The Marked Variable Predicate
    2.10. Generation of Statement Numbers

3.  TEMPLATE PROCESSING  ....................................  41

    3.1.  The Template Processing Command
          3.1.1.  Copying Files into Template Files
          3.1.2.  The Template File Stack
    3.2.  Insertion of Generated Declarations
    3.3. Special Template Variables

4.  OUTPUT REDIRECTION  .....................................  53

    4.1.  File Selection Commands
          4.1.1.  \fBgentranout\fR
          4.1.2.  \fBgentranshut\fR
    4.2.  Operations on the Output File Stack
          4.2.1.  \fBgentranpush\fR
          4.2.2.  \fBgentranpop\fR
    4.3.  Temporary Output Redirection

5.  MODIFICATION OF THE CODE GENERATION PROCESS  ............  64

    5.1.  Mode Switches
    5.2.  Variables
          5.2.1.  Target Language Selection
          5.2.2.  Expression Optimization & Segmentation Con-
                  trol
          5.2.3.  Temporary Variable Names &  Statement  Num-
                  bers
          5.2.4.  Code Formatting

6.  EXAMPLES  ...............................................  70

    6.1.  Example 1:  Interactive Code Generation
    6.2.  Example 2:  Template Processing
    6.3.  Example 3:  Segmentation & Optimization  Techniques
          6.3.1.  Code Generation
          6.3.2.  Segmentation
          6.3.3.  Expression Optimization
          6.3.4.  Generation of Temporary Variables  to  Sup-
                  press Simplification
          6.3.5.  Expression  Optimization  &  Generation  of
                  Temporary Variables
          6.3.6.  Comparison of Code Optimization  Techniques


    REFERENCES  .............................................  99


A.  TRANSLATABLE VAXIMA STATEMENTS & EXPRESSIONS  ........... 101

    A.1.  Formal Definition
    A.2.  Examples

B.  LIST OF GENTRAN FUNCTIONS, SWITCHES AND VARIABLES  ...... 108
.ft
.DE
