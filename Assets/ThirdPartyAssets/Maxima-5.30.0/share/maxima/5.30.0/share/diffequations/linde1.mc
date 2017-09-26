/*  THIS LITTLE PACKAGE SOLVES FIRST ORDER LINEAR
     ORDINARY DIFFERENTIAL EQUATIONS SUBJECT TO A 
     BOUNDARY CONDITION (B.C.) AT AN INITIAL POINT
       THE CALLING PROCEDURE IS
          IVPSOL(DIFFEQ,Y,X,A,BCEQ);
     WHERE DIFFEQ IS THE DIFFERENTIAL EQUATION TO BE SOLVED
            (E.G.:  X*'DIFF(Y,X)+2*Y=1)
           Y IS THE DEPENDENT VARIABLE
           X IS THE INDEPENDENT VARIABLE
           A IS THE POINT AT WHICH THE B.C. IS APPLIED
           BCEQ IS THE B.C. EQUATION (E.G.: Y=2)
*/
soldiff(eq1,y,x,a):=
     block([eq2,a1,a2,a3,a4,a5],
        eq2: lhs(eq1)-rhs(eq1),
        a1:  ratcoef(eq2,'diff(y,x)),
        a2:  ratcoef(eq2,y),
        a3:  eq2-a1*'diff(y,x)-a2*y,
        a4:  integrate(a2/a1,x),
        a5:  integrate(%e**a4*a3/a1,x),
           'const1/%e**a4-a5/%e**a4)$
ivpsol(diffeq,y,x,a,bceq):=
     block([z,dery,dya,ya,co1],
        z:   soldiff(diffeq,y,x,a),
        dery:diff(z,x,1),
        dya: subst(a,x,dery),
        ya:  subst(a,x,z),
        co1: solve(subst(ya,y,subst(dya,'diff(y,x),bceq)),'const1),
        if length(co1)>1 /* LISTP(CO1) */ then
           [print("SOLUTIONS ARE NOT UNIQUE, POSSIBLE SOLUTIONS ARE:
             "),
            map(lambda([l1],apply('display,[l1])),co1), return(z)] 
           else   if co1=[] then return(print("No solution for given initial
condition"),z) else subst(rhs(co1[1]),'const1,z))$
/* FIND RIGHT EIGENVECTORS WITH OTHER COMPONENT = 1
          I.E. X IS SOLUTION OF M*X=MU*X    */
evec(m,mu,modes):=block([equations,unknowns,x],
(for l:1 thru modes do[
     equations:[],
     unknowns:[],
/*     KILL(X), */
     x[l]:1,
     for i:1 thru modes do if not (i=l) then [
         unknowns:cons(x[i],unknowns),
         equations:cons(sum(m[i,j]*x[j],j,1,modes)
              =mu[l]*x[i],equations)],
     print(solve(equations,unknowns))]))$
/* CONSTRUCT 3 X 3 MATRIX WITH DESIRED EIGENVALUES */

consmat3(l1,l2,l3):=block([a,b,mat3,cp,cpm,sol1,l],
mat3:matrix([1,2,3],[b,3,a],[1,1,l1+l2+l3-4]),
cp:expand(-(l-l1)*(l-l2)*(l-l3)),
cpm:charpoly(mat3,l),globalsolve:true,
sol1:solve([ratcoef(cp,l)=ratcoef(cpm,l),
ratcoef(cp,l,0)=ratcoef(cpm,l,0)],[a,b]),
return(apply('ev,[mat3])))$
