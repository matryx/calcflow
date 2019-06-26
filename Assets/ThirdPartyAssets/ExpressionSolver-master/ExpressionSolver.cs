using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace AK
{
	/*

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

	*/

	public class ExpressionSolver
	{
		private static readonly int MaxCustomFunctionParamCount = 8;

		public enum UndefinedVariablePolicy
		{
			Error,
			DefineGlobalVariable,
			DefineExpressionLocalVariable
		}

		public UndefinedVariablePolicy undefinedVariablePolicy;

		private static Dictionary<string, double> immutableGlobalConstants = new Dictionary<string, double>()
		{
			{"e",System.Math.E},
			{"pi",System.Math.PI},
            {"π",System.Math.PI}
        };
		private Dictionary<string, Variable> globalConstants = new Dictionary<string, Variable>();
		private Dictionary<string, CustomFunction> customFuncs = new Dictionary<string, CustomFunction>();

		public ExpressionSolver()
		{
			undefinedVariablePolicy = UndefinedVariablePolicy.Error;
			AddCustomFunction("sin", delegate(double p) { return System.Math.Sin(p); },true);
			AddCustomFunction("cos", delegate(double p) { return System.Math.Cos(p); },true);
			AddCustomFunction("tan", delegate(double p) { return System.Math.Tan(p); },true);
			AddCustomFunction("abs", delegate(double p) { return System.Math.Abs(p); },true);
			AddCustomFunction("asin", delegate(double p) { return System.Math.Asin(p); },true);
			AddCustomFunction("acos", delegate(double p) { return System.Math.Acos(p); },true);
			AddCustomFunction("atan", delegate(double p) { return System.Math.Atan(p); },true);
			AddCustomFunction("atan2",2, delegate(double[] p) {	return System.Math.Atan2(p[0],p[1]); },true);
			AddCustomFunction("sqrt", delegate(double p) { return System.Math.Sqrt(p); },true);
			AddCustomFunction("sign", delegate(double p) { return System.Math.Sign(p); },true);
			AddCustomFunction("floor", delegate(double p) { return System.Math.Floor(p); },true);
			AddCustomFunction("ceil", delegate(double p) { return System.Math.Ceiling(p); },true);
			AddCustomFunction("min",2, delegate(double[] p) { return System.Math.Min(p[0],p[1]); },true);
			AddCustomFunction("max",2, delegate(double[] p) { return System.Math.Max(p[0],p[1]); },true);
			AddCustomFunction("sinh", delegate(double p) { return System.Math.Sinh(p); },true);
			AddCustomFunction("exp", delegate(double p) { return System.Math.Exp(p); },true);
			AddCustomFunction("cosh", delegate(double p) { return System.Math.Cosh(p); },true);
			AddCustomFunction("tanh", delegate(double p) { return System.Math.Tanh(p); },true);
			AddCustomFunction("log", delegate(double p) { return System.Math.Log(p); },true);
			AddCustomFunction("log10", delegate(double p) { return System.Math.Log10(p); },true);
			AddCustomFunction("round", delegate(double p) { return System.Math.Round(p); },true);
		}
		
		public void AddCustomFunction(string name, int paramCount, System.Func<double[],double> func, bool enableSymbolicationTimeEvaluation = false)
		{
			if (paramCount>MaxCustomFunctionParamCount)
			{
				throw new ESTooManyParametersException("Custom functions can have no more than " + MaxCustomFunctionParamCount + " parameters");
			}
			customFuncs[name] = new CustomFunction(name, paramCount, func, enableSymbolicationTimeEvaluation);
		}

		public void AddCustomFunction(string name, int paramCount, System.Func<object[],double> func, bool enableSymbolicationTimeEvaluation = false)
		{
			if (paramCount>MaxCustomFunctionParamCount)
			{
				throw new ESTooManyParametersException("Custom functions can have no more than " + MaxCustomFunctionParamCount + " parameters");
			}
			customFuncs[name] = new CustomFunction(name, paramCount, func, enableSymbolicationTimeEvaluation);
		}

		public void AddCustomFunction(string name, System.Func<double,double> func, bool enableSymbolicationTimeEvaluation = false)
		{
			customFuncs[name] = new CustomFunction(name, func, enableSymbolicationTimeEvaluation);
		}

		public void RemoveCustomFunction(string name)
		{
			customFuncs.Remove (name);
		}

		public Variable GetGlobalVariable(string name)
		{
			return globalConstants[name];
		}

		public Variable SetGlobalVariable(string name, double value)
		{
			Variable variable;
			if (globalConstants.TryGetValue(name, out variable))
			{
				if (variable.stringValue != null)
				{
					throw new ESParameterTypeChangedException("Can not change type of existing parameter " + name);
				}
				variable.value = value;
				return variable;
			}
			else
			{
				Variable v = new Variable(name,value);
				globalConstants.Add (name,v);
				return v;
			}
		}

		public Variable SetGlobalVariable(string name, string value)
		{
			if (value == null)
			{
				throw new System.ArgumentException("Null is not acceptable string parameter value.");
			}
			Variable variable;
			if (globalConstants.TryGetValue(name, out variable))
			{
				if (variable.stringValue == null)
				{
					throw new ESParameterTypeChangedException("Can not change type of existing parameter " + name);
				}
				variable.stringValue = value;
				return variable;
			}
			else
			{
				Variable v = new Variable(name,value);
				globalConstants.Add (name,v);
				return v;
			}
		} 

		public bool RemoveGlobalVariable(string name)
		{
			return globalConstants.Remove(name);
		}

		public void ClearGlobalVariables()
		{
			globalConstants.Clear();
		}

		public double EvaluateExpression(string formula)
		{
			return SymbolicateExpression(formula,(string[])null).Evaluate();
		}

		public Expression SymbolicateExpression(string formula, string localVariable) 
		{
			return SymbolicateExpression(formula,new string[1]{localVariable});
		}

		public Expression SymbolicateExpression(string formula, string[] localVariables = null) {
			Expression newExpression = new Expression();
			if (localVariables != null)
			{
				foreach (var localVariableName in localVariables)
				{
					if (localVariableName[0] == '$')
					{
						newExpression.SetVariable(localVariableName.Substring(1,localVariableName.Length-1).Trim(),"");
					}
					else
					{
						newExpression.SetVariable(localVariableName.Trim(),0.0);
					}
				}
			}

			formula = SolverTools.RemoveWhiteSpace(formula);

			// Check validity
			try
			{
				ValidityChecker.CheckValidity(formula);
			}
			catch (System.Exception ex)
			{
				throw ex;
			}

			Symbol s = Symbolicate(formula, 0, formula.Length,newExpression);
			newExpression.root = s;
			return newExpression;
		}

		static double ParseSymbols(SymbolList syms) 
		{
			bool transformNextValue = false;
			double sum = 0;
			double curTerm = 0;
			
			SymbolType prevOper = SymbolType.OperatorAdd;
			
			int len = syms.symbols.Count;
			var symbolList = syms.symbols;
			
			for (int i=0;i<len;i++) 
			{
				var s = symbolList[i];
				switch (s.type)
				{
					case SymbolType.RealValue:
					case SymbolType.SubExpression:
					case SymbolType.StringLiteral:
					case SymbolType.StringVariable:
					{
						double value;
						if (transformNextValue) 
						{
							var funcSymbol = symbolList[i-1];
							switch (funcSymbol.type) 
							{
								case SymbolType.Pow:
								{
									value = System.Math.Pow(GetSymbolValue(s),GetSymbolValue(symbolList[i+1]));
									i++;
									break;
								}
								case SymbolType.FuncCustom:
								{
									var customFunc = (CustomFunction)funcSymbol.ptr;
									if (customFunc.paramCount == 1 && customFunc.func1d != null)
									{
										value = customFunc.Invoke(GetSymbolValue(s));
									}
									else if (customFunc.funcmo != null)
									{
										object[] p = new object[MaxCustomFunctionParamCount];
										p[0] = (!s.IsStringType()) ? (object)GetSymbolValue(s) : (object)s.stringValue;
										for (int g=1;g<customFunc.paramCount;g++)
										{
											p[g] = (!symbolList[i+1].IsStringType()) ? (object)GetSymbolValue(symbolList[i+1]) : (object)symbolList[i+1].stringValue;
											i++;
										}
										value = customFunc.Invoke(p);
									}
									else
									{
										double[] p = new double[MaxCustomFunctionParamCount];
										p[0] = GetSymbolValue(s);
										for (int g=1;g<customFunc.paramCount;g++)
										{
											p[g] = GetSymbolValue(symbolList[i+1]);
											i++;
										}
										value = customFunc.Invoke(p);
									}
									break;
								}
								default:
									throw new System.Exception("Very unexpected parse error.");
							}
							transformNextValue = false;
						}
						else
						{
							// The value cant be a string because they appear only as parameters to functions
							value = GetSymbolValue(s);
						}
						
						switch (prevOper)
						{
							case SymbolType.OperatorMultiply:
								curTerm *= value;
								break;
							case SymbolType.OperatorDivide:
								curTerm /= value;
								break;
							case SymbolType.OperatorAdd:
								sum += curTerm;
								curTerm = value;
								break;
							default:
								throw new System.Exception("Very unexpected parse error.");
						}
						prevOper = SymbolType.OperatorMultiply;
						break;
					}
					case SymbolType.OperatorDivide:
					case SymbolType.OperatorAdd:
						prevOper = s.type;
						break;
					case SymbolType.Pow:
					case SymbolType.FuncCustom:
						transformNextValue = true;
						break;
					default:
						throw new System.Exception("Unable to parse symbols.");
				}
			}
			// Remember to add the final term to sum
			return sum + curTerm;
		}

		public static double GetSymbolValue(Symbol s) 
		{
			return (s.type == SymbolType.RealValue) ? (s.ptr == null ? s.value : ((Variable)s.ptr).value ) : ParseSymbols((SymbolList)s.ptr);
		}

		Symbol SymbolicateValue(string formula, int begin, int end, Expression exp)
		{
			if (formula[begin] == '+')
			{
				begin++;
			}

			// Check for string value
			if (formula[begin] == '\'' && formula[end - 1] == '\'')
			{
				var svalue = formula.Substring(begin+1,end-begin-2).Replace("\\'","'");
				return new Symbol(svalue);
			}

			int depth=0;
			for (int k = begin; k < end; k++)
			{
				if (formula[k]=='(')
					depth++;
				else if (formula[k]==')')
					depth--;
				else if (depth == 0 && formula[k] == '^')
				{
					// Check for small integer powers: they will be done using multiplication instead!
					Symbol lhs = Symbolicate(formula,begin,k,exp);
					Symbol rhs = Symbolicate(formula,k+1,end,exp);
					var newSubExpression = new SymbolList();
					if (end-k-1 == 1 && lhs.type == SymbolType.RealValue && formula.Substring(k+1,end-k-1)=="2") 
					{
						// Second power found
						newSubExpression.Append(lhs);
						newSubExpression.Append(lhs);
					}
					else if (end-k-1 == 1 && lhs.type == SymbolType.RealValue && formula.Substring(k+1,end-k-1)=="3")
					{
						// Second power found
						newSubExpression.Append(lhs);
						newSubExpression.Append(lhs);
						newSubExpression.Append(lhs);
					}
					else
					{
						newSubExpression.Append(new Symbol(SymbolType.Pow));
						newSubExpression.Append(lhs);
						newSubExpression.Append(rhs);
					}
					Symbol newSymbol = new Symbol(SymbolType.SubExpression);
					newSymbol.subExpression = newSubExpression;
					return newSymbol;
				}
			}
			
			if (formula[begin] == '(' && formula[end - 1] == ')')
			{
				var s = Symbolicate(formula, begin + 1, end - 1,exp);
				s.Simplify();
				return s;
			}

			double valueAsRealNumber;
			if (double.TryParse(formula.Substring(begin,end-begin),out valueAsRealNumber))
			{
				return new Symbol(valueAsRealNumber);
			}
			
			// Check if the value is transformed by a function
			if (formula[end-1]==')') {
				int i = begin;
				while (i < end-1) {
					if (formula[i]=='(') {
						break;
					}
					i++;
				}

				string funcName = formula.Substring(begin,i-begin);
				CustomFunction customFunc;
				if (customFuncs.TryGetValue(funcName,out customFunc))
				{
					int requiredParameterCount = customFunc.paramCount;
					int foundParameterCount = SolverTools.CountParameters(formula,begin,end);
					if (requiredParameterCount == foundParameterCount) {
						if (requiredParameterCount == 1) {
							SymbolList newSubExpression = new SymbolList();
							newSubExpression.Append(new Symbol(customFunc));
							newSubExpression.Append(Symbolicate(formula,i+1,end-1,exp));
							return new Symbol(newSubExpression);
						}
						else {
							List<SolverTools.IntPair> parameters = SolverTools.ParseParameters(formula,i,end);
							SymbolList newSubExpression = new SymbolList();
							newSubExpression.Append(new Symbol(customFunc));
							for (int k=0;k<requiredParameterCount;k++) {
								Symbol p = Symbolicate(formula,parameters[k].first,parameters[k].second,exp);
								newSubExpression.Append(p);
							}
							
							Symbol newSymbol = new Symbol(SymbolType.SubExpression);
							newSymbol.subExpression = newSubExpression;
							return newSymbol;
							
						}
					}
					else 
					{
						throw new ESInvalidParametersException(customFunc.name + " expects " + requiredParameterCount + " parameters, " + foundParameterCount + " given.");
					}
				}
				else
				{
					throw new ESInvalidFunctionNameException(funcName);
				}
			}
			
			var valueName = formula.Substring(begin,end-begin);

			// Then a local constant specific to our expression
			Variable variable;
			if (exp.constants.TryGetValue(valueName,out variable)) {
				return new Symbol(variable);
			}

			// Non immutable globals
			if (globalConstants.TryGetValue(valueName,out variable)) {
				return new Symbol(variable);
			}

			// Immutable globals
			double constDouble;
			if (immutableGlobalConstants.TryGetValue(valueName, out constDouble)) 
			{
				return new Symbol(constDouble);
			}

			// Found an unknown value name. Check policy to see what to do.
			Variable v = null;
			switch (undefinedVariablePolicy)
			{
				case UndefinedVariablePolicy.DefineExpressionLocalVariable:
					v = new Variable(valueName,0);
					exp.constants.Add(valueName,v);
					return new Symbol(v);
				case UndefinedVariablePolicy.DefineGlobalVariable:
					v = new Variable(valueName,0);
					globalConstants.Add(valueName,v);
					return new Symbol(v);
				default:
					throw new ESUnknownExpressionException(valueName);
			}
		}

		Symbol SymbolicateMonome(string formula, int begin, int end, Expression exp)
		{
			var symbols = new SymbolList();
			int sign = 0;
			int i = begin - 1;
			int currentTermBegin = begin;
			int numValues = 0;
			int currentDepth = 0;
			double constMultiplier = 1.0;
			bool divideNext = false;
			bool constMultiplierUsed = false;
			for (;;) 
			{
				i++;
				if (i == end || (currentDepth == 0 && i > begin && (formula[i] == '*' || formula[i] == '/')))
				{
					numValues++;
					
					// Unless we are dealing with a monome, symbolicate the term
					Symbol newSymbol = SymbolicateValue(formula, formula[currentTermBegin] == '-' ? currentTermBegin + 1 : currentTermBegin, i,exp);
					// Check if we can simplify the generated symbol
					if (newSymbol.IsImmutableConstant() && newSymbol.IsRealValueType()) 
					{
						// Constants are multiplied/divided together
						if (divideNext)
							constMultiplier /= GetSymbolValue(newSymbol);
						else
							constMultiplier *= GetSymbolValue(newSymbol);
						constMultiplierUsed = true;
					}
					else
					{
						if (divideNext)
							symbols.Append(new Symbol(SymbolType.OperatorDivide));
						newSymbol.Simplify();
						symbols.Append(newSymbol);
					}

					if (i == end) {
						break;
					}
					divideNext = formula[i] == '/';
					currentTermBegin = i + 1;
				}
				else if (formula[i] == '(')
				{
					currentDepth++;
				}
				else if (formula[i] == ')')
				{
					currentDepth--;
				}
				else if (formula[i] == '-' && currentDepth == 0 && !(i>begin && formula[i-1] == '^') )
				{
					sign++;
				}
			}

			// If the generated monome has negative number of minus signs, then we append *-1 to end of the list, or if the preceding symbol is constant real number that is part of a monome, we multiply it.
			if (sign % 2 == 1)
			{
				constMultiplier =-constMultiplier;
			}
			if (constMultiplierUsed || sign % 2 == 1)
			{
				// Add the const multiplier to the expression
				if (symbols.Length>0 && symbols.first.type==SymbolType.OperatorDivide)
				{
					// Put to the begin of the expression we are building
					symbols.symbols.Insert(0,new Symbol(constMultiplier));
				}
				else if (symbols.Length > 0 && symbols.last.type == SymbolType.SubExpression && symbols.last.IsMonome())
				{
					// Add inside the last subexpression
					SymbolList leftSideExpression = symbols.last.subExpression;
					if (leftSideExpression.last.type==SymbolType.RealValue && leftSideExpression.last.IsImmutableConstant())
					{
						leftSideExpression.SetSymbolAtIndex(leftSideExpression.Length-1,new Symbol(leftSideExpression.last.value*constMultiplier));
					}
					else
					{
						leftSideExpression.Append(new Symbol(constMultiplier));
					}
				}
				else
				{
					// Put to the end of the expression we are building
					symbols.Append(new Symbol(constMultiplier));
				}
			}

			// Check if the final monome is just a real number, in which case we don't have to return a subexpression type
			if (symbols.Length == 1 && symbols.first.IsImmutableConstant() && symbols.first.IsRealValueType())
			{
				return symbols.first.type == SymbolType.RealValue ? symbols.first : new Symbol(GetSymbolValue(symbols.first));
			}
			Symbol s = new Symbol(SymbolType.SubExpression);
			s.subExpression = symbols;
			s.Simplify();
			return s;
		}

		Symbol Symbolicate(string formula, int begin, int end, Expression exp) 
		{
			var symbols = new SymbolList();

            int i = begin - 1;
            int currentTermBegin = 0;
            int currentDepth = 0;
            try
            {
                currentTermBegin = formula[begin] == '+' ? begin + 1 : begin;
                currentDepth = 0;
            }
            catch (Exception e)
            {
                Debug.Log("this is where");
            }
			
			
			for (;;) 
			{
				i++;
				if (i == end || (currentDepth == 0 && i > begin && (formula[i - 1] != '*' && formula[i - 1] != '/') && (formula[i] == '+' || formula[i] == '-')))
				{
					symbols.Append(SymbolicateMonome(formula, currentTermBegin, i,exp));
					if (i == end)
					{
						break;
					}
					else {
						// The sign of the term is included in the next monome only if its minus
						currentTermBegin = (formula[i] == '-') ? i : i + 1;
						symbols.Append(new Symbol(SymbolType.OperatorAdd));
					}
				}
				else if (formula[i] == '(') 
				{
					currentDepth++;
				}
				else if (formula[i] == ')') 
				{
					currentDepth--;
				}
				else if (formula[i] == '^') 
				{
					i = SolverTools.ParseUntilEndOfExponent(formula,i+1,end) - 1;
				}
			}
			
			// If at this point we only have one real number left, just return it as a simple value.
			if (symbols.Length == 1 && symbols.first.type == SymbolType.RealValue) 
			{
				return symbols.first;
			}
			
			// We don't have that single expression, but:
			// Now that we are here, we have symbol list which consists of only addition operators and value types. This is a great place to sum constant values together!
			double constantSum = 0;
			bool addedConstants = false;

			for (int j = 0; j < symbols.Length; j++)
			{
				Symbol s = symbols.getSymbol(j);
				if (s.IsImmutableConstant() && s.IsRealValueType()) {
					constantSum += s.value;
					addedConstants = true;
					if (j == symbols.Length - 1)
					{
						// Destroy preceding +
						symbols.symbols.RemoveAt (j);
						break;
					}
					symbols.symbols.RemoveAt(j);
					symbols.symbols.RemoveAt(j);
					j--;
				}
				else 
				{
					// Skip the following + symbol
					j++;
				}
			}
			if (addedConstants) 
			{
				if (symbols.Length > 0 && symbols.getSymbol(symbols.Length - 1).IsRealValueType()) 
				{
					symbols.Append(new Symbol(SymbolType.OperatorAdd));
				}
				symbols.Append(new Symbol(constantSum));
			}

			// Finally, if the symbolicated sum is just a single real number, even varying, return just a simple symbol
			if (symbols.Length == 1 && symbols.getSymbol(0).type == SymbolType.RealValue)
			{
				Symbol s = symbols.getSymbol(0);
				return s;
			}
			
			// Optimization: get rid of unnecessary jumps to subexpressions
			for (int j=0;j<symbols.Length;j++)
			{
				var s = symbols.getSymbol(j);
				if (s.type==SymbolType.SubExpression)
				{
					var subExpression = s.subExpression;
					int subExpressionLength = subExpression.Length;
					s.CopyValuesFrom(subExpression.first);
					for (int k=1;k<subExpressionLength;k++)
					{
						symbols.InsertBefore(j+k,subExpression.getSymbol(k));
					}
					j += subExpressionLength;
				}
			}
			
			// We have turned the formula into a subexpression symbol
			Symbol returnSymbol = new Symbol(symbols);
			returnSymbol.Simplify();
			return returnSymbol;
		}
	}

}