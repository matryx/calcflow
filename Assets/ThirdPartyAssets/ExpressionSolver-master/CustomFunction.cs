using VoxelBusters.RuntimeSerialization;
namespace AK
{
    [RuntimeSerializable(null, true, true)]
    public class CustomFunction
    {
        public string name;

        public delegate double FuncMD(double[] dList);
        public delegate double FuncMO(object[] dList);
        public delegate double Func1D(double dVal);

        public enum paramType
        {
            OBJECT,
            DOUBLE,
            DOUBLEL
        }

        public paramType type;

        public event FuncMD funcmd;
        public event FuncMO funcmo;
        public event Func1D func1d;
        public int paramCount;
        public bool enableSymbolicationTimeEvaluation;

        public CustomFunction(string name, int paramCount, FuncMD func, bool enableSymbolicationTimeEvaluation)
        {
            this.type = paramType.DOUBLEL;
            this.funcmd += func;
            this.enableSymbolicationTimeEvaluation = enableSymbolicationTimeEvaluation;
            this.paramCount = paramCount;
            this.name = name;
        }

        public CustomFunction(string name, int paramCount, FuncMO func, bool enableSymbolicationTimeEvaluation)
        {
            this.type = paramType.OBJECT;
            this.funcmo = func;
            this.enableSymbolicationTimeEvaluation = enableSymbolicationTimeEvaluation;
            this.paramCount = paramCount;
            this.name = name;
        }

        public CustomFunction(string name, Func1D func, bool enableSymbolicationTimeEvaluation)
        {
            this.type = paramType.DOUBLE;
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
            return func1d != null ? func1d(x) : funcmd(new double[] { x });
        }

    }

}