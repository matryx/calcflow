# ExpressionSolver
A C# mathematical expression solver with Unity3D compatibility. Supports custom functions (that may also accept string parameters) and named constants.

Usage:

    AK.ExpressionSolver solver = new AK.ExpressionSolver();
    var exp = solver.SymbolicateExpression("1+1");
    double result = exp.Evaluate(); // Returns 2.0
    solver.EvaluateExpression("1+1"); // Also returns 2.0

Expression is symbolicated only once: subsequent calls to Evaluate are fast. If you are going to evaluate the expression only once, you can do without Expression objects and just call solver.EvaluateExpression(string formula) like we did in the example above.

There are two kinds of variables: global and expression-local. Global variables are shared between all Expressions created using the same parser object:

    ExpressionSolver solver = new ExpressionSolver();
    solver.SetGlobalVariable("test",1);
    var exp1 = solver.SymbolicateExpression("test+1");
    AssertSameValue(2.0,exp1.Evaluate());
    solver.SetGlobalVariable("test",2);
    var exp2 = solver.SymbolicateExpression("test+1");
    AssertSameValue(3.0,exp2.Evaluate());
    AssertSameValue(exp1.Evaluate(),exp2.Evaluate());

This can be problematic in a multithreaded environment. Therefore, ExpressionSolver also provides expression-local variables that you can
specify when calling SymbolicateExpression:

	ExpressionSolver solver = new ExpressionSolver();
	var exp1 = solver.SymbolicateExpression("test+1",new string[]{"test"});
	exp1.SetVariable("test",1.0);
	var exp2 = solver.SymbolicateExpression("test+1","test"); // If you define only one variable, you don't need to use the string[] format
	exp2.SetVariable("test",2.0);
    solver.SetGlobalVariable("test",1000); 
    variable.AssertSameValue(exp1.Evaluate(),2); // Prefer the exp-local if there is a name clash
	AssertSameValue(exp2.Evaluate(),3);
	var exp3 = solver.SymbolicateExpression("test^2");
	AssertSameValue(exp3.Evaluate(),1000*1000);

You can choose how ExpressionSolver should handle undefined variables. The default policy is to throw an exception:

    ExpressionSolver solver = new ExpressionSolver();
    try {
        var exp1 = solver.SymbolicateExpression("test");
		throw new System.Exception("We shouldn't be here!");
    }
	catch (AK.ESUnknownExpressionException) {
	   // Expected behaviour
	}

You can override the default policy:

	solver.undefinedVariablePolicy = ExpressionSolver.UndefinedVariablePolicy.DefineGlobalVariable;
	var exp2 = solver.SymbolicateExpression("test2");
    AssertSameValue(solver.GetGlobalVariable("test2").value,0);
	AssertSameValue(exp2.Evaluate(),0);

	solver.undefinedVariablePolicy = ExpressionSolver.UndefinedVariablePolicy.DefineExpressionLocalVariable;
	var exp3 = solver.SymbolicateExpression("sin(test3)");
	var test3 = exp3.GetVariable("test3");
	AssertSameValue(test3.value,0);
	test3.value = System.Math.PI/2;
	AssertSameValue(exp3.Evaluate(),1);

There are two ways to change values stored in a variable. You can of course use the solver.SetGlobalVariable/exp.SetVariable functions 
as in the above examples. If you want to do a huge number of evaluations, you may want to use solver.GetGlobalVariable/exp.GetVariable to
get reference to the variable. With the reference, you can change the value without using hash tables:

    const int N = 1000000;
    ExpressionSolver solver = new ExpressionSolver();
    var exp = solver.SymbolicateExpression("1/2^i","i");
    double sum = 0;
    for (int i=0;i<N;i++)
    {
        exp.SetVariable("i",i);
        sum += exp.Evaluate();
    }
    AssertSameValue(sum,2);
    sum = 0;
    var variable = exp.GetVariable("i");
    for (int i=0;i<N;i++)
    {
        variable.value = i;
        sum += exp.Evaluate();
    }
    AssertSameValue(sum,2);

On my laptop, the latter loop was around 25% faster than the former.


Custom functions that take up to 8 parameters are also supported. If the function takes only one parameter, this is how you define
the function:

    solver.AddCustomFunction("inverse",delegate(double p) { return 1.0/p; },true);

Functions with multiple parameters take their input as an array of doubles. Remember to give also the number of parameters your function
expects:

    solver.AddCustomFunction("average", 2, delegate(double[] p) { return 0.5*(p[0]+p[1]); },true);

The last parameter (true in above examples) specifies whether the function always produces same output given same input. When such
functions have constant parameters, they can be evaluated at symbolication time. Therefore, the following loop runs very fast:

    var exp = solver.SymbolicateExpression("sin(cos(tan(312^3)))");
    for (int i=0;i<10000;i++)
        exp.Evaluate();

However, this is not acceptable if your function produces random output. In that case, you must tell ExpressionSolver that
this optimization should not be used with the function:

    solver.AddCustomFunction("Rnd",2, delegate(double[] p) {
			return UnityEngine.Random.Range((float)p[0],(float)p[1]);
		},false);
    // Now we get different value on each iteration
    var exp = solver.SymbolicateExpression("Rnd(0,1)");
    for (int i=0;i<10000;i++)
        exp.Evaluate();

Since the last parameter passed to AddCustomFunction was false, the custom Rnd function was not evaluated at symbolication time
and we got the desired results. False is also the default parameter here.

The following functions are supported by default:

    sin,cos,tan,sinh,cosh,tanh,asin,acos,atan,atan2,abs,min,max,exp,log,log10,
    ceil,floor,round,sqrt

Some examples:

    ExpressionSolver solver = new ExpressionSolver();
    solver.SetGlobalVariable("zero",0);
    var exp1 = solver.SymbolicateExpression("sin(pi/2)-cos(zero)");
    AssertSameValue(exp1.Evaluate(),0);
    var exp2 = solver.SymbolicateExpression("2*e^zero - exp(zero)");
    AssertSameValue(exp2.Evaluate(),1);
    var exp3 = solver.SymbolicateExpression("log(e^6)");
    AssertSameValue(exp3.Evaluate(),6);
    var exp4 = solver.SymbolicateExpression("sqrt(2)-2^0.5");
    AssertSameValue(exp4.Evaluate(),0);

Custom functions can also accept string parameters. Parameters to these functions are passed as an array of C# objects. For example:

    ExpressionSolver solver = new ExpressionSolver();
    solver.AddCustomFunction("strlen",1, delegate(object[] p) {
		return ((string)p[0]).Length;
	},true);
    var exp1 = solver.SymbolicatExpression("strlen('1234')");
    double len = exp1.Evaluate(); // Returns 4.0

Escape character inside the string literal is backslash. To insert that inside a C# string literal, you of course need to enter it twice:

    var exp2 = solver.SymbolicatExpression("strlen('\\'')");
    double len = exp2.Evaluate(); // Returns 1.0, the length of "'"

String functions make it easy to access your Unity scene from ExpressionSolver formulas. For example:

    solver.AddCustomFunction("distBetweenGameObjects",2, delegate(object[] p) {
		var go1 = UnityEngine.GameObject.Find((string)p[0]);
        var go2 = UnityEngine.GameObject.Find((string)p[1]);
        if (go1 != null && go2 != null)
        {
            return UnityEngine.Vector3.Distance(go1.transform.position,go2.transform.position);
        }
        else
        {
            return -1.0;
        }
	});
    var dist = ExpressionSolver.EvaluateExpression("distBetweenGameObjects('GameObject1','GameObject2')");

You can also combine string and double parameters:

    solver.AddCustomFunction("strnlen()",2, delegate(object[] p) {
        return System.Math.Min((double)((string)p[0]).Length,System.Math.Round(p)((double)p[1]));
	});
    
Both global and expression-local variables can also assume string values. When using expression-local string variables, add a dollar
sign before the variable when calling SymbolicateExpression:

	solver.SetGlobalVariable("stringvar","12345");
	exp = solver.SymbolicateExpression("strlen(stringvar)");
    AssertSameValue(exp.Evaluate(),5);
    exp2 = solver.SymbolicateExpression("strlen(stringvar2)","$stringvar2");
    exp2.SetVariable("stringvar2","stringtest");
    AssertSameValue(exp2.Evaluate(),10);
    

Strings can only appear as parameters to string accepting functions and string concatenation is not currently supported.

    solver.EvaluateExpression("'abba'"); // Error: strings can not appear as independent values
    solver.EvaluateExpression("strlen('ab'+'ba'"); // Error: no string concatenation

Have fun with ExpressionSolver!


ExpressionSolver is licensed under the MIT license.


Copyright (c) 2015 Antti Kuukka



Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:



The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.



THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
