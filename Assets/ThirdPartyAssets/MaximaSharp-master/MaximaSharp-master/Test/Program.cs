using MaximaSharp;
using System;
using System.Linq.Expressions;

namespace Test
{
    class Program
    {
        static void Main(string[] args)
        {
            Expression<Func<double, double>> f = x => 3 * Math.Pow(x, 2) + 2 * x + Math.Pow(Math.Cos(x), 2) + Math.Pow(Math.Sin(x), 2);
            Expression<Func<double, double>> g = x => 2 * x + 5 * 2;
            Expression<Func<double, double, double>> h = (y, z) => y + z;

            Console.WriteLine(f.Times(g).Simplify());
            Console.WriteLine(f.Over(g).Simplify());

            Maxima.GnuPlot(@"plot x+5*cos(x)");
            Maxima.GnuPlot(@"
                set parametric 
                set pm3d depthorder hidden3d
                set isosamples 30, 20
                splot [-pi:pi][-pi:pi] cos(u)*(cos(v)+3), sin(u)*(cos(v)+3), sin(v) w pm
            ");
            
            Console.ReadLine();
        }
    }
}
