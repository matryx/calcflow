using System.Collections;
using System.Collections.Generic;

namespace AK
{
	public class SymbolList
	{
		public List<Symbol> symbols = new List<Symbol>();
		
		public override string ToString ()
		{
			var l = this;
			string r = "";
			for (int i=0;i<l.symbols.Count;i++) {
				var s = l.getSymbol(i);
				switch (s.type) {
				case SymbolType.Pow:
					r+=(l.getSymbol(i+1));
					r+=("^");
					r+=(l.getSymbol(i+2));
					i+=2;
					break;
				case SymbolType.FuncCustom: {
					r += s.customFunc.name + "(";
					r+=(l.getSymbol(i+1));
					if (s.customFunc.paramCount>1) {
						for (int j=1;j<s.customFunc.paramCount;j++) {
							r += ",";
							r+=l.getSymbol(i+1+j);
						}
					}
					r += ")";
					i+= s.customFunc.paramCount;
				}
					break;
				case SymbolType.RealValue:
				case SymbolType.SubExpression:
					r+=(s);
					if (i<l.symbols.Count-1) {
						if (l.getSymbol(i+1).IsRealValueType()) {
							r+=("*");
						}
					}
					break;
				default:
					r+=(s);
					break;
				}
			}
			return r;
		}
		
		public int Length
		{
			get { return symbols.Count; }
		}
		
		public void SetSymbolAtIndex(int index, Symbol s)
		{
			symbols[index] = s;
		}
		
		public Symbol last 
		{
			get { return symbols[symbols.Count-1]; }
		}
		
		public Symbol first {
			get { return symbols[0]; }
		}
		
		public int Append(Symbol s) {
			symbols.Add(s);
			return 1;
		}
		
		public void InsertBefore(int index, Symbol s) {
			symbols.Insert(index,s);
		}
		
		public Symbol getSymbol(int index) {
			return symbols[index];
		}
		
	}
}
