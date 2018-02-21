using VoxelBusters.RuntimeSerialization;
namespace AK
{
	[RuntimeSerializable(null, true, true)]
	public class Variable
	{
		public double value;
		public string stringValue = null;

		public string name { get; private set; }
		
		public Variable(string name)
		{
			this.value=0;
			this.name = name;
		}
		
		public Variable(string name, double v)
		{
			this.value=v;
			this.name = name;
		}
		
		public Variable(string name, string s)
		{
			this.stringValue=s;
			this.name = name;
		}

	}

}