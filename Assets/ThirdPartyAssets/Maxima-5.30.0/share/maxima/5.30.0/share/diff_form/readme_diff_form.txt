                           PREFACE

This is a differential form package slightly singular.
This package is intended to provide a working knowledge
through calculating differential forms for students finished advanced calc.
In short,not to gather formulas, but to calc and induce them from calculation.
In this field we need various other knowledge about math and physics.
I hope this package is useful for maxima people.
In detail you should read nice books such that....

Differential Forms with Applications to the Physical Sciences
written by Harley Flanders ISBN 0-486-66169-5 Dover (reprint)

Differential Forms and Connections 
written by R.W.R.Darling ISBN 0-521-46800-0 Cambridge university Press 

The Geometry of Physics An Introduction 
written by Theodore Frankel ISBN 0-521-53927-7 Cambridge university Press

Differential Forms
written by Henri Cartan ISBN 0-486-45010-4 Dover (reprint)

Geometrical methods of mathematical physics
written by Bernand F.Schutz  Cambridge university Press 

I think this package is intermediate between vect package and tensor package 
in maxima.  So in this time,I did not implement bundle spaces, though especially 
 need S1-bundle. I don't like too formalism such as other CAS's diff-form package.
Because non-usability of these package prevents from using bundle spaces,as
bundle is a natural tool for peoples using it for thinking.
My todo-list is to implement bundles with natural usability.
I welcome All comments, advices,bug reports.to maxima mailing list
or me (go.maxima@gmail.com) 


A little new thing I implemented is to use clifford operator for seeking integral factor.
If exist,(f^-1 is integral factor),d(w)=f^(-1)df @ w. then @--->&(clifford)
d(w)/w is possible,because we calculate w & w -->A (some numbers),in clifford algebra
1/w=w/A. d(w)/w---> d(w)&w/A. when  &-->@,use some quantization,@ is independent from metric.
when calculating with &,we introduce metric parameter automatically 
 for example u1,u2,u3,such that dx&dx=u1,...then u1-->0,u2-->0,u3=\=0,&-->@. 
This calculation is illegal only within  differential forms,
but regal clifford-grassman algebra. Usually in such a case we use an Ideal on differential forms.
But I think it is essential that limit of metric breaks clifford structure,but cannot affect
grassman structure,so after this limit, we can change & to @ . 
See example.txt.

hodge star operator is written with clifford algebra in definition,but user need not to
recognize that. hodge star is named h_st().
poisson braket with symplectic form is much useful,if you may explicitly  give hamilton 
operator.
To multiple unit pseudo scalar is almost same hodge star.(at most,differnt sign + or -)
this J() is defined and used to define antidifferential operator antid().


                                 INTRODUCTION    

There is two way for starting. 
One way is load("diff_form.mac"). This mean to use no grobal coords,basis others.
As it were clean start. All work may be done in f_star() or fstar_with_clf().
you  can change coords freely.
Other way is batch("new_cartan_test4.bat").This mean to use global coords,basis.
But to change coords locally is allways possible in f_star() or fstar_with_clf().
A example of this grobal way is seen in lorentz_example.txt 

It is important for using this package well that we awake to distinction between
global variables and local variables. for example
(%i21) fstar_with_clf([x,y,z],[x,y,z],(r:x^2+y^2+z^2,d((x*Dy@Dz+y*Dz@Dx+z*Dx@Dy)/(sqrt(r)))));

for usage
f_star(coordinates,calc area)
fstar_with_clf(new coordinates,representing standered coordinate with new one,calc area)

now basis is[Dx,Dy,Dz],norm_table,scale_factor ,all this local. but  r:x^2+y^2+z^2 is not 
local,only r is global. (%i22) format(%,%poly(Dx,Dy,Dz),factor);
after this,if foget that r was global,
(%i23) fstar_with_clf([r,phi,th],[r*sin(th)*cos(phi),r*sin(th)*sin(phi),r*cos(th)],
d(x)@d(y)@d(z)));
error #0: f_star(newcoords=[z^2+y^2+x^2,phi,th],.......
so (%i24) kill(r); then (%i23) is OK.
nest2([d,h_st,d],f*d(x)) is equal d(h_st(d(f*d(x))))
d(x) is equal Dx,so Dx@Dy is d(x)@d(y),but internaly in d(x)@d(y) exterior derivative are done.

ALL files
diff_form.mac one initial file ,(only local coordinate environment)

new_cartan_test4.mac another initial file,(global coordinate and local coords environment)

cartan_new.lisp 
derived from share/calculus/cartan.lisp add & operator and others (for future use)

f_star_test4.mac
main functions f_star(),fstar_with_clf(),inner(),Lie(),these functions run under any dimension

hodge_test3.mac
hodge star operator h_st() under any dimension

helpfunc.mac
utilities or help function vtof1(),vtof2(),J(),antid(),others

poisson.mac
poisson bracket on simplectic manifold (dim 2*n) p_braket()

frobenius.mac
calculate integral factor.trans_toexact(),see example.txt

curvture2.mac
add_tan(),cross(),make_tan(),only dim 2,or 3,this is experimental,see surface_example.txt

readme_diff_form.txt  this file.

lorentz_example.txt
surface_example.txt
example.txt

