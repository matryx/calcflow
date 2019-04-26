using System.Collections;
using System.Collections.Generic;

namespace AK
{
	public static class ExpressionSolverTests
	{
		public static void AssertSameValue(double f1, double f2)
		{
			var diff = System.Math.Abs(f1-f2);
			if (diff>0.0000001f)
			{
				throw new System.Exception("ExpressionSolverTest failed");
			}
		}

		public static void AssertSameValue(string s1, string s2)
		{
			if (s1 != s2)
			{
				UnityEngine.Debug.Log(s1 + " vs " + s2);
				throw new System.Exception("ExpressionSolverTest failed");
			}
		}

		public static void Assert(bool cond)
		{
			if (!cond)
			{
				throw new System.Exception("ExpressionSolverTest failed");
			}
		}

		public static void TestComplexFormulas()
		{
			ExpressionSolver solver = new ExpressionSolver();
			{
				var exp = solver.SymbolicateExpression("sin(1/(0.5*x))","x");
				var x = 21.0;
				exp.SetVariable("x",x);
				AssertSameValue(exp.Evaluate(),System.Math.Sin(1/(0.5*x)) );
			}
			{
				var exp = solver.SymbolicateExpression("(1+1)+.5");
				AssertSameValue(exp.Evaluate(),2.5f);
			}
			{
				var exp = solver.SymbolicateExpression("sin(2/(1-0.5*x))","x");
				var x = 21.0;
				exp.SetVariable("x",x);
				AssertSameValue(exp.Evaluate(),System.Math.Sin(2/(1-0.5*x)) );
			}
			{
				var exp = solver.SymbolicateExpression("sin((2/(1-0.5*x))/2)","x");
				var x = 21.0;
				exp.SetVariable("x",x);
				AssertSameValue(exp.Evaluate(),System.Math.Sin((2/(1-0.5*x))/2));
			}
			{
				var exp = solver.SymbolicateExpression("1/(1+sin((2/(1-0.5*x))/2))","x");
				var x = 21.0;
				exp.SetVariable("x",x);
				AssertSameValue(exp.Evaluate(),1/(1+System.Math.Sin((2/(1-0.5*x))/2)));
			}
			{
				var exp = solver.SymbolicateExpression(" (1-(3*(x/( (x+22)/x)-1*2*x))^0.5)/(x+1) ","x");
				var x = -21.0;
				exp.SetVariable("x",x);
				AssertSameValue(exp.Evaluate(), (1-System.Math.Pow(3*(x/( (x+22)/x)-1*2*x),0.5))/(x+1) );
			}
			{
				var exp = solver.SymbolicateExpression("(1+1/x)^x","x");
				var x = 30000000.0;
				exp.SetVariable("x",x);
				AssertSameValue(exp.Evaluate(),System.Math.E);
			}
		}

		public static void Run()
		{
			TestNames();
			TestComplexFormulas();
			TestStringFuncs();
			TestGlobalConstants();
			TestExpLocalConstants();
			TestUndefinedVariablePolicies();
			TestSum();
			TestFuncs();
			TestWhiteSpaceRemoval();
		}

		private static void TestNames()
		{
			ExpressionSolver solver = new ExpressionSolver();
			try
			{
				solver.EvaluateExpression("0hakka");
				throw new System.Exception("Test failed");
			}
			catch (ESInvalidNameException)
			{
				// As expected
			}
			try
			{
				solver.EvaluateExpression("0.0.0");
				throw new System.Exception("Test failed");
			}
			catch (ESInvalidNameException)
			{
				// As expected
			}
		}

		public static void TestStringFuncs()
		{
			ExpressionSolver solver = new ExpressionSolver();
			solver.AddCustomFunction("strlen",1, delegate(object[] p) {
				return ((string)p[0]).Length;
			},true);
			var exp = solver.SymbolicateExpression("strlen('123')");
			AssertSameValue(exp.Evaluate(),3.0);
			exp = solver.SymbolicateExpression("strlen('12\\'3')");
			AssertSameValue(exp.Evaluate(),4.0);
			exp = solver.SymbolicateExpression("strlen('12\\'3 4')");
			AssertSameValue(exp.Evaluate(),6.0);
			solver.AddCustomFunction("strlen2",2, delegate(object[] p) {
				return ((string)p[0]).Length*(double)p[1];
			});
			exp = solver.SymbolicateExpression("strlen2('12\\'3 4',2.5)");
			AssertSameValue(exp.Evaluate(),6.0*2.5);

			string[] erroneousStrings = new string[]{"strlen(''')","strlen('''')","''"};
			foreach (var errorString in erroneousStrings)
			{
				try 
				{
					exp = solver.SymbolicateExpression(errorString);
					throw new System.Exception("ExpressionSolverTest failed");
				}
				catch (ESSyntaxErrorException)
				{
					// Parameters were not given correctly - syntax error expected
				}
				catch (System.Exception)
				{
					throw new System.Exception("ExpressionSolverTest failed");
				}
			}

			// Because strlen should be evaluated at symbolication time, the following should reduce to one real value symbol:
			exp = solver.SymbolicateExpression("(1+strlen('123')+1)/strlen('12345')");
			Assert(exp.root.type == SymbolType.RealValue);
			AssertSameValue(exp.root.value,1);
			// But if one of the parameters is not constant, then we cant do it:
			exp = solver.SymbolicateExpression("strlen('123')+x", new string[]{"x"});
			Assert(exp.root.type == SymbolType.SubExpression);

			// Test string variables. Both exp-local and global
			exp = solver.SymbolicateExpression("strlen(stringVariableTest)","$stringVariableTest");
			exp.SetVariable("stringVariableTest","test");
			AssertSameValue(exp.Evaluate(),4);
			try
			{
				exp.SetVariable("stringVariableTest",121);
				Assert(false);
			}
			catch (ESParameterTypeChangedException)
			{
			}
			solver.SetGlobalVariable("striva","123");
			exp = solver.SymbolicateExpression("strlen( (  striva  )  )/3");
			AssertSameValue(exp.Evaluate(),1);
			try
			{
				solver.SetGlobalVariable("striva",42141.0);
				Assert(false);
			}
			catch (ESParameterTypeChangedException)
			{
			}
		}

		public static void TestWhiteSpaceRemoval()
		{
			string formula = "ab cd";
			AssertSameValue(SolverTools.RemoveWhiteSpace(formula),"abcd");
			formula = "'ab cd'";
			AssertSameValue(SolverTools.RemoveWhiteSpace(formula),"'ab cd'");
			formula = " 'ab cd' ";
			AssertSameValue(SolverTools.RemoveWhiteSpace(formula),"'ab cd'");
			formula = " 'ab\\' cd ' ";
			AssertSameValue(SolverTools.RemoveWhiteSpace(formula),"'ab\\' cd '");
		}

		public static void TestFuncs()
		{
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
			var exp5 = solver.SymbolicateExpression("exp(log(6))");
			AssertSameValue(exp5.Evaluate(),6);
			var rnd = new System.Random();
			solver.AddCustomFunction("Rnd1",2, delegate(double[] p) {
				return p[0] + (p[1]-p[0])*(rnd.NextDouble());
			},false);
			var exp6 = solver.SymbolicateExpression("Rnd1(0,1)");
			var firstRnd = exp6.Evaluate();
			int iter = 0;
			while (true)
			{
				var secondRnd = exp6.Evaluate();
				if (firstRnd != secondRnd)
				{
					break;
				}
				iter++;
				if (iter==10000)
				{
					// Probability of this happening is miniscule if everything works as it should
					throw new System.Exception("ExpressionSolverTest failed");
				}
			}
			solver.AddCustomFunction("Rnd2",2, delegate(double[] p) {
				return p[0] + (p[1]-p[0])*(rnd.NextDouble());
			},true);
			var exp7 = solver.SymbolicateExpression("Rnd2(0,1)");
			AssertSameValue(exp7.Evaluate(),exp7.Evaluate());
			var exp8 = solver.SymbolicateExpression("cos(0)+1*2");
			AssertSameValue(exp8.Evaluate(),3);
			solver.AddCustomFunction("dist",5, delegate(double[] p) {
				return System.Math.Pow( (p[2]-p[0])*(p[2]-p[0]) + (p[3]-p[1])*(p[3]-p[1])    ,p[4]);
			},true);
			var exp9 = solver.SymbolicateExpression("dist(3*x,(4*x),+6*x,-1*x,sin(x))","x");
			double x = 21;
			exp9.SetVariable("x",x);
			AssertSameValue(exp9.Evaluate(),System.Math.Pow( (3*x-6*x)*(3*x-6*x)+(4*x+x)*(4*x+x),System.Math.Sin(x)  ));
		}

		public static void TestSum()
		{
			const int N = 10000;
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
		}

		public static void TestUndefinedVariablePolicies()
		{
			ExpressionSolver solver = new ExpressionSolver();
			try
			{
				solver.SymbolicateExpression("test");
				throw new System.Exception("ExpressionSolverTest failed");
			}
			catch (AK.ESUnknownExpressionException)
			{
				// Expected behaviour
			}

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
		}

		public static void TestGlobalConstants()
		{
			ExpressionSolver solver = new ExpressionSolver();
			solver.SetGlobalVariable("test",1);
			var exp1 = solver.SymbolicateExpression("test+1");
			AssertSameValue(2.0,exp1.Evaluate());
			solver.SetGlobalVariable("test",2);
			var exp2 = solver.SymbolicateExpression("test+1");
			AssertSameValue(3.0,exp2.Evaluate());
			AssertSameValue(exp1.Evaluate(),exp2.Evaluate());
		}

		public static void TestExpLocalConstants()
		{
			ExpressionSolver solver = new ExpressionSolver();
			var exp1 = solver.SymbolicateExpression("test+1",new string[]{"test"});
			exp1.SetVariable("test",1.0);
			var exp2 = solver.SymbolicateExpression("test+1","test"); // If you define only one variable, you don't need to use the string[] format
			exp2.SetVariable("test",2.0);
			solver.SetGlobalVariable("test",1000); // If there is name clash with a exp-local variable, we prefer the exp-local variable.
			AssertSameValue(exp1.Evaluate(),2);
			AssertSameValue(exp2.Evaluate(),3);
			var exp3 = solver.SymbolicateExpression("test^2");
			AssertSameValue(exp3.Evaluate(),1000*1000);
		}

	}

}