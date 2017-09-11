using System.Collections;
using System.Collections.Generic;

namespace AK
{
	public class Expression
	{
		public Symbol root;
		public Dictionary<string, Variable> constants = new Dictionary<string, Variable>();

		public Variable SetVariable(string name, double value)
		{
			Variable v;
			if (constants.TryGetValue(name,out v))
			{
				if (v.stringValue != null)
				{
					throw new ESParameterTypeChangedException("Can not change type of existing parameter " + name);
				}
				v.value = value;
				return v;
			}
			v = new Variable(name,value);
			constants.Add(name,v);
			return v;
		}

		public Variable SetVariable(string name, string value)
		{
			Variable v;
			if (constants.TryGetValue(name,out v))
			{
				if (v.stringValue == null)
				{
					throw new ESParameterTypeChangedException("Can not change type of existing parameter " + name);
				}
				v.stringValue = value;
				return v;
			}
			v = new Variable(name,value);
			constants.Add(name,v);
			return v;
		}

		/// <summary>
		/// Get reference to a variable that has been set up already.
		/// </summary>
		/// <returns>The variable or null if variable is not found.</returns>
		/// <param name="name">Name of the variable.</param>
		public Variable GetVariable(string name)
		{
			return constants[name];
		}

		public override string ToString()
		{
			if (root.type == SymbolType.SubExpression)
			{
				var s = root.ToString();
				return s.Substring(1,s.Length-2);
			}
			else
			{
				return root.ToString();
			}
		}

		public double Evaluate()
		{
			return ExpressionSolver.GetSymbolValue(root);
		}
	}
}