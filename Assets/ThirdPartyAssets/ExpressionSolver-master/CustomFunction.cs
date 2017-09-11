namespace AK
{
	
	public class CustomFunction
	{
		public string name;
		public System.Func<double[],double> funcmd;
		public System.Func<object[],double> funcmo;
		public System.Func<double,double> func1d;
		public int paramCount;
		public bool enableSymbolicationTimeEvaluation;
		
		public CustomFunction(string name, int paramCount, System.Func<double[],double> func, bool enableSymbolicationTimeEvaluation)
		{
			this.funcmd = func;
			this.enableSymbolicationTimeEvaluation = enableSymbolicationTimeEvaluation;
			this.paramCount = paramCount;
			this.name = name;
		}

		public CustomFunction(string name, int paramCount, System.Func<object[],double> func, bool enableSymbolicationTimeEvaluation)
		{
			this.funcmo = func;
			this.enableSymbolicationTimeEvaluation = enableSymbolicationTimeEvaluation;
			this.paramCount = paramCount;
			this.name = name;
		}

		public CustomFunction(string name, System.Func<double,double> func, bool enableSymbolicationTimeEvaluation)
		{
			this.func1d = func;
			this.enableSymbolicationTimeEvaluation = enableSymbolicationTimeEvaluation;
			this.paramCount = 1;
			this.name = name;
		}

		public double Invoke(double[] p)
		{
			return funcmd(p);
		}

		public double Invoke(object[] p)
		{
			return funcmo(p);
		}

		public double Invoke(double x)
		{
			return func1d != null ? func1d(x) : funcmd(new double[]{x});
		}

	}
	
}