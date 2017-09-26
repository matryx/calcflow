.nr LL 6.5i
.nr FL 6.0i
.ND
.DS L
.ft CR
  STATEMENT TYPE   |   USER LEVEL EXAMPLE   |        LISP LEVEL EXAMPLE
-------------------+------------------------+-----------------------------------
                   |                        |
Assignments        |                        |
                   |                        |
  - simple         |p : a*x^2 + b*x + c$    |((msetq) p
                   |                        |         ((mplus)
                   |                        |          ((mtimes) a
                   |                        |                    ((mexpt) x 2))
                   |                        |          ((mtimes) b x)
                   |                        |          c))
                   |                        |
  - matrix         |m : matrix( [u, v],     |((msetq) m
                   |            [w, x] )$   |         (($matrix) ((mlist) u v)
                   |                        |                    ((mlist) w x)))
                   |                        |
                   |                        |
Control Structures |                        |
                   |                        |
  Loops            |                        |
                   |                        |
  - for-step-thru  |for i:1 step 2 thru 9   |((mdo) i 1 2 nil 9 nil
                   |    do p : p + a[i]$    |       ((msetq) p ((mplus) p
                   |                        |                           ((a) i))
                   |                        |       ))
                   |                        |
  - for-next-thru  |for n:2 next n*2        |((mdo) n 2 nil ((mtimes) n 2) 500
                   |        thru 500        |       nil
                   |    do s : s + n$       |       ((msetq) s ((mplus) s n)))
                   |                        |
                   |                        |
                   |                        |
                   |                        |
  - while          |while f(x)>=0           |((mdo) nil nil nil nil nil
                   |      do x : x + 0.25$  |       ((mnot) ((mgeqp) ((f) x) 0))
                   |                        |       ((msetq) x ((mplus) x 0.25))
                   |                        |)
                   |                        |
  - unless         |unless f(x)>=0          |((mdo) nil nil nil nil nil
                   |       do x : x + 0.25$ |       ((mgeqp) ((f) x) 0)
                   |                        |       ((msetq) x ((mplus) x 0.25))
                   |                        |)
                   |                        |
  Conditional      |                        |
  Transfer of      |                        |
  Control          |                        |
                   |                        |
  - if-then        |if tot>=0               |((mcond) ((mgeqp) tot 0)
                   |   then flag : true$    |         ((msetq) flag t)
                   |                        |         t
                   |                        |         $false)
                   |                        |
  - if-then-else   |if tot>=0               |((mcond) ((mgeqp) tot 0)
                   |   then flag : true     |         ((msetq) flag t)
                   |   else flag : false$   |         t
                   |                        |         ((msetq) flag nil))
                   |                        |
                   |                        |
                   |                        |
  Unconditional    |                        |
  Transfer of      |                        |
  Control          |                        |
                   |                        |
  - goto           |go(loop)$               |((mgo) loop)
                   |                        |
  - call           |calcz(a,b,c,z)$         |((calcz) a b c z)
                   |                        |
  - return         |return(x^2)$            |((mreturn) ((mexpt) x 2))
                   |                        |
                   |                        |
Statement Sequences|                        |
& Groups           |                        |
                   |                        |
  - sequence       |( u : x^2, v : y^2 )$   |((mprogn) ((msetq) u ((mexpt) x 2))
                   |                        |          ((msetq) v ((mexpt) y 2))
                   |                        |)
                   |                        |
  - group          |block( u : x^2,         |((mprog) ((msetq) u ((mexpt) x 2))
                   |       v : y^2 )$       |         ((msetq) v ((mexpt) y 2)))
                   |                        |
                   |                        |
                   |                        |
I/O Statements     |                        |
                   |                        |
  - input          |x : readonly("Enter x")$|((msetq) x (($readonly) |&Enter x|)
                   |                        |)
                   |                        |
  - output         |print("x = ", x)$       |(($print) |&x = | x)
                   |                        |
Subprograms        |                        |
                   |                        |
  - function       |f(a,b,c,x) :=           |((mdefine) ((f) a b c x)
                   |  block( z : a*b*c + x, | ((mprog)
                   |         return(z) )$   |  ((msetq) z ((mplus)
                   |                        |              ((mtimes) a b c)
                   |                        |              x))
                   |                        |  ((mreturn) z)))
                   |                        |
  - subroutine     |f(a,b,c,x) :=           |((mdefine) ((f) a b c x)
                   |  block( z : a*b*c + x, | ((mprog)
                   |         print(z) )$    |  ((msetq) z ((mplus)
                   |                        |              ((mtimes) a b c)
                   |                        |              x))
                   |                        |  (($print) z)))
-------------------+------------------------+-----------------------------------
.ft
.DE
