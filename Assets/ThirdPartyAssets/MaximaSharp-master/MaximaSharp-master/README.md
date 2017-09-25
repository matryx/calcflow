MaximaSharp
===========
MaximaSharp is a simple library that uses Maxima to perform symbolic algebra, plot functions, and do basic operations with C#'s lambda functions, expressions, and strings. Both examples and Maxima are included.

What is Maxima?
---------------
> [Maxima](http://maxima.sourceforge.net/ "Maxima") is a system for the manipulation of symbolic and numerical expressions, including differentiation, integration, Taylor series, Laplace transforms, ordinary differential equations, systems of linear equations, polynomials, and sets, lists, vectors, matrices, and tensors. Maxima yields high precision numeric results by using exact fractions, arbitrary precision integers, and variable precision floating point numbers. Maxima can plot functions and data in two and three dimensions.

Using MaximaSharp
-----------------
Given the following lambda expressions declared in C#:
```csharp
Expression<Func<double, double>> f = x => 3 * Math.Pow(x, 2) + 2 * x 
			+ Math.Pow(Math.Cos(x), 2) + Math.Pow(Math.Sin(x), 2);
Expression<Func<double, double>> g = x => 2 * x + 5 * 2;
Expression<Func<double, double, double>> h = (y, z) => 3 * y + z;
```

### Simplifying ###
Simplifying functions is easy:
```csharp
Console.WriteLine(f.Simplify());
Console.WriteLine(g.Simplify());
Console.WriteLine(h.Simplify());
// Output:
// x => (((3 * (x ^ 2)) + (2 * x)) + 1)
// x => (2 * (x + 5))
// (y, z) => (z + (3 * y))
```

### Differentiating ###
It's also possible to take the derivative of functions:
```csharp
Console.WriteLine(f.Differentiate());
Console.WriteLine(g.Differentiate());
Console.WriteLine(h.Differentiate("y"));
// Output:
// x => ((6 * x) + 2)
// x => 2
// (y, z) => 3
```

### Integrating ###
Definite and indefinite integrals can also be found:
```csharp
Console.WriteLine(f.Integrate().Simplify());
Console.WriteLine(f.Integrate(0, 2));
Console.WriteLine(g.Integrate());
Console.WriteLine(h.Integrate("y"));
// Output:
// x => (x * (((x ^ 2) + x) + 1))
// x => 14
// x => ((x ^ 2) + (10 * x))
// (y, z) => ((y * z) + ((3 * (y ^ 2)) / 2))
```

### Plotting ###
Plot functions easily with gnuplot:
```csharp
Maxima.GnuPlot(@"plot x+5*cos(x)");
Maxima.GnuPlot(@"
	set parametric 
	set pm3d depthorder hidden3d
	set isosamples 30, 20
	splot [-pi:pi][-pi:pi] cos(u)*(cos(v)+3), sin(u)*(cos(v)+3), sin(v) w pm
");
Console.ReadLine();
```
Produces the following graphs:

![Plot of x + 5 * cos(x)](https://raw.github.com/sgbj/MaximaSharp/master/wavy.png)
![Plot of cos(u)*(cos(v)+3), sin(u)*(cos(v)+3), sin(v)](https://raw.github.com/sgbj/MaximaSharp/master/cheerio.png)

### More stuff ###
Evaluate functions:
```csharp
Console.WriteLine(f.At(5));
Console.WriteLine(g.At(10));
// Output:
// 86
// 30
```

Perform basic operations on functions:
```csharp
Console.WriteLine(g.Plus(h));
Console.WriteLine(g.Minus(h));
Console.WriteLine(f.Times(g).Simplify());
Console.WriteLine(f.Over(g).Simplify());
// Output:
// (x, y, z) => (((2 * x) + 10) + ((3 * y) + z))
// (x, y, z) => (((2 * x) + 10) - ((3 * y) + z))
// x => ((2 * (x + 5)) * (((3 * (x ^ 2)) + (2 * x)) + 1))
// x => ((((3 * (x ^ 2)) + (2 * x)) + 1) / (2 * (x + 5)))
```
<br/>
***

__Note:__ evaluating functions and basic operations are performed without the use of Maxima.

***
<br/>
Evaluate strings with Maxima:
```csharp
Console.WriteLine(Maxima.Eval("x + 2 + 2 * x + 3 * 5"));
// Output:
// 3*x+17
```

Convert strings back into expressions:
```csharp
var expr = Maxima.ToExpression("double, double", "x", "10 * x + 5 * cos(x)");
Console.WriteLine(expr);
Console.WriteLine(expr.Differentiate().Simplify().At(0));
// Output:
// x => ((10 * x) + (5 * Cos(x)))
// 10
```
<br/>
***

__Example:__ converting user input to an expression that can be differentiated, integrated, plotted, or evaluated.

***
<br/>
