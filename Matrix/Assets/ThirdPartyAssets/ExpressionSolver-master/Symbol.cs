using System.Collections;

namespace AK
{
	
	public enum SymbolType
	{
		Empty,
		RealValue,
		OperatorAdd,
		OperatorMultiply,
		OperatorDivide,
		SubExpression,
		Pow,
		StringLiteral,
		StringVariable,
		FuncCustom,
	};
	
	public class Symbol
	{
		public SymbolType type;
		public double _value;
		public object ptr;

		public Variable variable 
		{ 
			get { 
				return (Variable)ptr;
			}

			set
			{ 
				ptr = value;
			}
		}

		public string stringValue 
		{ 
			get { 
				return type == SymbolType.StringLiteral ? (string)ptr : ((Variable)ptr).stringValue;
			}
			
			set
			{ 
				ptr = value;
			}
		}

		public SymbolList subExpression 
		{ 
			get
			{ 
				return (SymbolList)ptr;
			} 

			set
			{
				ptr = value;
			} 
		}

		public CustomFunction customFunc 
		{ 
			get 
			{ 
				return (CustomFunction)ptr;
			} 

			set
			{
				ptr = value;
			} 
		}

		// Test if value of the symbol is independent of variables.
		public bool IsImmutableConstant()
		{
			if (type == SymbolType.RealValue)
			{
				if (variable != null)
					return false;
				return true;
			}
			else if (type == SymbolType.StringLiteral)
			{
				return true;
			}
			else if (type == SymbolType.SubExpression)
			{
				return IsSymbolListImmutableConstant(subExpression);
			}
			else
			{
				return false;
			}
		}

		public bool IsMonome()
		{
			var s = this;
			if (s.type==SymbolType.RealValue)
			{
				return true;
			}
			else if (s.type==SymbolType.SubExpression) 
			{
				var syms = s.subExpression;
				for (int i=0;i<syms.Length;i++)
				{
					var r = syms.getSymbol(i);
					if (r.type!=SymbolType.RealValue && r.type!=SymbolType.SubExpression)
					{
						return false;
					}
				}
				return true;
			}
			return false;
		}

		public void Simplify() {
			// ((x)) ==> (x)
			if (type == SymbolType.SubExpression)
			{
				if (subExpression.Length != 1)
				{
					return;
				}
				if (subExpression.first.type == SymbolType.SubExpression)
				{
					// Get pointer to sub-subexpression
					SymbolList subSubExpression = subExpression.symbols[0].subExpression;
					subExpression = subSubExpression;
				}
				else if (subExpression.first.type == SymbolType.RealValue || subExpression.first.type == SymbolType.StringLiteral || subExpression.first.type == SymbolType.StringVariable)
				{
					// We have single real number surrounded by parenthesis, it can become a real number
					CopyValuesFrom(subExpression.first);
				}
			}
		}
		
		private bool IsSymbolListImmutableConstant(SymbolList l)
		{
			var len = l.Length;
			for (int k = 0; k < len; k++)
			{
				var s = l.getSymbol(k);
				if (s.type == SymbolType.RealValue)
				{
					if (s.variable != null)
					{
						return false;
					}
				}
				else if (s.type == SymbolType.FuncCustom && !s.customFunc.enableSymbolicationTimeEvaluation)
				{
					return false;
				}
				else if (s.type == SymbolType.StringVariable)
				{
					return false;
				}
				else if (s.type == SymbolType.SubExpression)
				{
					if (!IsSymbolListImmutableConstant(s.subExpression))
					{
						return false;
					}
				}
			}
			return true;
		}
		
		public void CopyValuesFrom(Symbol o)
		{
			type = o.type;
			_value = o._value;
			ptr = o.ptr;
		}
		
		public double value 
		{
			get
			{
				return variable == null ? _value : ((Variable)ptr).value;
			}
		}
		
		public bool IsRealValueType()
		{
			return type == SymbolType.RealValue || type == SymbolType.SubExpression;
		}

		public bool IsStringType()
		{
			return type == SymbolType.StringLiteral || type == SymbolType.StringVariable;
		}
		
		public Symbol(SymbolType type, double va)
		{
			this.type = type;
			_value = va;
			variable = null;
		}
		
		public Symbol(SymbolType type)
		{
			this.type = type;
		}

		public Symbol(SymbolList subExpression)
		{
			this.type = SymbolType.SubExpression;
			this.subExpression = subExpression;
		}
		
		public Symbol(double value)
		{
			type = SymbolType.RealValue;
			_value = value;
		}

		public Symbol(string stringValue)
		{
			type = SymbolType.StringLiteral;
			ptr = stringValue;
		}
		
		public Symbol(Variable ptrToConstValue)
		{
			type = ptrToConstValue.stringValue != null ? SymbolType.StringVariable : SymbolType.RealValue;
			variable = ptrToConstValue;
		}
		
		public Symbol(CustomFunction func)
		{
			type = SymbolType.FuncCustom;
			customFunc = func;
		}
		
		public override string ToString()
		{
			switch (type) 
			{
				case SymbolType.RealValue:
				case SymbolType.StringVariable:
					if (variable != null)
					{
						return variable.name;
					}
					return _value.ToString();
				case SymbolType.OperatorAdd:
					return "+";
				case SymbolType.StringLiteral:
					return "\'" + stringValue + "\'";
				case SymbolType.OperatorMultiply:
					return "*";
				case SymbolType.OperatorDivide:
					return "/";
				case SymbolType.Pow:
					return "pow";
				case SymbolType.SubExpression:
					return "("+subExpression.ToString()+")";
				case SymbolType.Empty:
					return "(null)";
				case SymbolType.FuncCustom:
					return customFunc.name;
			}
			return "";
		}
	}

}
