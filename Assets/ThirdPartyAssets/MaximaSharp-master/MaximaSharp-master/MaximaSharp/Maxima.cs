using Microsoft.VisualBasic;
using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Linq.Expressions;

namespace MaximaSharp
{
    public static class Maxima
    {
        public static List<string> Functions = new List<string> { "pow(x, y) := x^y" };
        private static Process Process = NewMaxima();

        private static Process NewMaxima()
        {
            if (Process != null) Process.Dispose();
            var startInfo = new ProcessStartInfo(@"Maxima-5.30.0\lib\maxima\5.30.0\binary-gcl\maxima.exe", @"-eval ""(cl-user::run)"" -f -- -very-quiet")
            {
                WorkingDirectory = @"Maxima-5.30.0\bin\",
                UseShellExecute = false,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            };
            startInfo.EnvironmentVariables.Add("maxima_prefix", @"..\..\Maxima-5.30.0");
            return Process.Start(startInfo);
        }

        public static string Eval(string expr)
        {

            Process.StandardInput.WriteLine(string.Format("{0}$ grind({1});", string.Join("$", Functions), expr.ToLower()));
            var result = Process.StandardOutput.ReadLine();
            while (!result.EndsWith("$"))
                result += Process.StandardOutput.ReadLine();
            Process.StandardOutput.ReadLine();
            if (result.EndsWith("$")) return result.TrimEnd('$');
            Process = NewMaxima();
            throw new Exception(string.Format("Unexpected result: {0}", result));
        }

        public static LambdaExpression ToExpression(string types, string parameters, string code)
        {
            try
            {
                return VBCodeProvider.CreateProvider("VB", new Dictionary<string, string>() { { "CompilerVersion", "v4.0" } })
                    .CompileAssemblyFromSource(new CompilerParameters(new[] { "System.Core.dll" }), string.Format(
               @"   Imports System
                    Imports System.Linq.Expressions
                    Public Class Program 
                        Public Shared Lambda As Expression(Of Func(Of {0})) = Function({1}) {2}
                    End Class
                ", types, parameters, code.Replace("log", "Math.Log").Replace("sin", "Math.Sin").Replace("cos", "Math.Cos")))
                    .CompiledAssembly.GetType("Program").GetField("Lambda").GetValue(null) as LambdaExpression;
            }
            catch (Exception ex)
            {
                throw new Exception(string.Format("Failed to convert to expression: {0}", code), ex);
            }
        }

        private static LambdaExpression EvalToExpression(this LambdaExpression expr, string format, params object[] args)
        {
            return ToExpression(string.Join(", ", expr.Type.GetGenericArguments().Select(t => t.Name)),
                                string.Join(", ", expr.Parameters.Select(p => p.Name)),
                                Eval(string.Format(format, args)));
        }

        public static LambdaExpression Simplify(this LambdaExpression expr)
        {
            return EvalToExpression(expr, "factor(fullratsimp(trigsimp({0})))", expr.Body);
        }

        public static LambdaExpression Differentiate(this LambdaExpression expr, string wrt = "x")
        {
            return EvalToExpression(expr, "diff({0}, {1})", expr.Body, wrt);
        }

        public static LambdaExpression Integrate(this LambdaExpression expr, string wrt = "x")
        {
            return EvalToExpression(expr, "integrate({0}, {1})", expr.Body, wrt);
        }

        public static LambdaExpression Integrate(this LambdaExpression expr, int from, int to, string wrt = "x")
        {
            return EvalToExpression(expr, "integrate({0}, {1}, {2}, {3})", expr.Body, wrt, from, to);
        }

        public static LambdaExpression Plus(this LambdaExpression f, LambdaExpression g)
        {
            return Expression.Lambda(Expression.Add(f.Body, g.Body), f.Parameters.Union(g.Parameters, new ParameterEqualityComparer()));
        }

        public static LambdaExpression Minus(this LambdaExpression f, LambdaExpression g)
        {
            return Expression.Lambda(Expression.Subtract(f.Body, g.Body), f.Parameters.Union(g.Parameters, new ParameterEqualityComparer()));
        }

        public static LambdaExpression Times(this LambdaExpression f, LambdaExpression g)
        {
            return Expression.Lambda(Expression.Multiply(f.Body, g.Body), f.Parameters.Union(g.Parameters, new ParameterEqualityComparer()));
        }

        public static LambdaExpression Over(this LambdaExpression f, LambdaExpression g)
        {
            return Expression.Lambda(Expression.Divide(f.Body, g.Body), f.Parameters.Union(g.Parameters, new ParameterEqualityComparer()));
        }

        public static object At(this LambdaExpression f, params object[] args)
        {
            return f.Compile().DynamicInvoke(args);
        }

        public static void GnuPlot(string s)
        {
            var gnuplot = Process.Start(new ProcessStartInfo(@"Maxima-5.30.0\gnuplot\gnuplot.exe")
            {
                UseShellExecute = false,
                RedirectStandardInput = true,
                RedirectStandardOutput = true
            });
            gnuplot.StandardInput.WriteLine(s);
        }
        
        private class ParameterEqualityComparer : IEqualityComparer<ParameterExpression>
        {
            public bool Equals(ParameterExpression x, ParameterExpression y)
            {
                return x.Name == y.Name;
            }

            public int GetHashCode(ParameterExpression obj)
            {
                return obj.Name.GetHashCode();
            }
        }
    }
}
