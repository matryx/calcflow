.nr LL 6.5i
.nr FL 6.0i
.ND
.DS L
.ft CR
|             FORTRAN CODE             |        RATFOR CODE
+--------------------------------------+----------------------------
|                                      |
|                                      |
|                                      |
|      p=a*x**2+b*x+c                  |p=a*x**2+b*x+c
|                                      |
|                                      |
|                                      |
|                                      |
|                                      |
|                                      |
|      m(1,1)=u                        |m(1,1)=u
|      m(1,2)=v                        |m(1,2)=v
|      m(2,1)=w                        |m(2,1)=w
|      m(2,2)=x                        |m(2,2)=x
|                                      |
|                                      |
|                                      |
|                                      |
|                                      |
|      do 25001 i=1,9,2                |do i=1,9,2
|         p=p+a(i)                     |    p=p+a(i)
|25001 continue                        |
|                                      |
|                                      |
|      n=2.0                           |for (n=2.0;!n>500.0;n=n*2.0)
|25002 if (n.gt.500.0) goto 25003      |    s=s+n
|         s=s+n                        |
|         n=n*2.0                      |
|         goto 25002                   |
|25003 continue                        |
|                                      |
|25004 if (.not.f(x).ge.0.0) goto 25005|while (f(x)>=0.0)
|         x=x+0.25                     |    x=x+0.25
|         goto 25004                   |
|25005 continue                        |
|                                      |
|25006 if (f(x).ge.0.0) goto 25007     |while (!f(x)>=0.0)
|         x=x+0.25                     |    x=x+0.25
|         goto 25006                   |
|25007 continue                        |
|                                      |
|                                      |
|                                      |
|                                      |
|                                      |
|      if (.not.tot.ge.0.0) goto 25008 |if (tot>=0.0)
|          flag=.true.                 |    flag=.true.
|25008 continue                        |
|                                      |
|                                      |
|      if (.not.tot.ge.0.0) goto 25009 |if (tot>=0.0)
|         flag=.true.                  |    flag=.true.
|         goto 25010                   |else
|25009 continue                        |    flag=.false.
|         flag=.false.                 |
|25010 continue                        |
|                                      |
|                                      |
|                                      |
|                                      |
|                                      |
|      goto 25011                      |goto 25011
|                                      |
|      call calcz(a,b,c,z)             |call calcz(a,b,c,z)
|                                      |
|      \fIfunctionname\fR=x**2               |return(x**2)
|      return                          |
|                                      |
|                                      |
|                                      |
|                                      |
|      u=x**2                          |u=x**2
|      v=y**2                          |v=y**2
|                                      |
|                                      |
|      u=x**2                          |{
|      v=y**2                          |    u=x**2
|                                      |    v=y**2
|                                      |}
|                                      |
|                                      |
|                                      |
|      write(*,*) "Enter x"            |write(*,*) "Enter x"
|      read(*,*) x                     |read(*,*) x
|                                      |
|      write(*,*) "x = ", x            |write(*,*) "x = ", x
|                                      |
|                                      |
|                                      |
|      function f(a,b,c,x)             |function f(a,b,c,x)
|      z=a*b*c+x                       |z=a*b*c+x
|      f=z                             |return(z)
|      return                          |end
|      end                             |
|                                      |
|                                      |
|      subroutine f(a,b,c,x)           |subroutine f(a,b,c,x)
|      z=a*b*c+x                       |z=a*b*c+x
|      write(*,*) z                    |write(*,*) z
|      return                          |return
|      end                             |end
|                                      |
+--------------------------------------+--------------------------------
.ft
.DE
