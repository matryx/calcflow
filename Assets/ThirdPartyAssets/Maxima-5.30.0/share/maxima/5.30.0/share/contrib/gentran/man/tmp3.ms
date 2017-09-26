.nr LL 6.5i
.nr FL 6.0i
.ND
.DS L
.ft CR
|            C CODE
+-------------------------------
|
|
|
|p=a*power(x,2)+b*x+c;
|
|
|
|
|
|
|m[1][1]=u;
|m[1][2]=v;
|m[2][1]=w;
|m[2][2]=x;
|
|
|
|
|
|for (i=1;!(i>9);i=i+2)
|    p=p+a[i];
|
|
|
|for (n=2.0;!(n>500.0);n=n*2.0)
|    s=s+n;
|
|
|
|
|
|while (f(x)>=0.0)
|    x=x+0.25;
|
|
|
|while (!(f(x)>=0.0))
|    x=x+0.25;
|
|
|
|
|
|
|
|if (tot>=0.0)
|    flag=1;
|
|
|
|if (tot>=0.0)
|    flag=1;
|else
|    flag=0;
|
|
|
|
|
|
|
|goto loop;
|
|calcz(a,b,c,z);
|
|return(power(x,2));
|
|
|
|
|
|u=power(x,2);
|v=power(y,2);
|
|
|{
|    u=power(x,2);
|    v=power(y,2);
|}
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|-------------------------------
.ft
.DE
