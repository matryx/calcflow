using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace AK {

	public static class SolverTools
	{
		public struct IntPair
		{
			public int first;
			public int second;
			public IntPair(int first, int second)
			{
				this.first = first;
				this.second = second;
			}
		}

		private static bool IsWhiteSpaceChar(char c)
		{
			return c == ' ' || c == '\t' || c == '\n' || c == '\r';
		}

		public static string RemoveWhiteSpace(string formula)
		{
			int l = formula.Length;
			StringBuilder sb = new StringBuilder(l);
			for (int i=0;i<l;i++)
			{
				char c = formula[i];
				if (c=='\'')
				{
					sb.Append(c);
					for (int j=i+1;j<l;j++) 
					{
						char d = formula[j];
						if (d=='\'' && formula[j-1] != '\\')
						{
							i++;
							break;
						}
						i++;
						sb.Append(d);
					}
				}
				if (!IsWhiteSpaceChar(c))
				{
					sb.Append(c);
				}
			}
			return sb.ToString();
		}
		
		public static List<IntPair> ParseParameters(string formula, int begin, int end) 
		{
			List<IntPair> r = new List<IntPair>();
			
			int currentParamBegin = -1;
			int depth = 0;
			
			for (int i=begin;i<end;i++) {
				if (formula[i] == '(') {
					if (depth == 0) {
						// First parameters
						currentParamBegin = i+1;
					}
					depth++;
				}
				else if (formula[i] == ')') {
					depth--;
					if (depth == 0) {
						r.Add (new IntPair(currentParamBegin,i));
					}
				}
				else if (formula[i] == ',' && depth == 1) {
					r.Add (new IntPair(currentParamBegin,i));
					currentParamBegin = i+1;
				}
			}
			return r;
		}
		
		public static int CountParameters(string formula,int begin,int end) {
			int depth = 0;
			int r = 1;
			for (int i=begin;i<end;i++) {
				if (formula[i] == '(') {
					depth++;
				}
				else if (formula[i] == ')') {
					depth--;
				}
				else if (formula[i] == ',' && depth == 1) {
					r++;
				}
			}
			return r;
		}
		
		public static int ParseUntilEndOfExponent(string formula, int begin, int end) {
			int currentDepth = 0;
			for (int i=begin;i<end;i++) {
				if (formula[i] == '(') {
					currentDepth++;
				}
				else if (formula[i] == ')') {
					currentDepth--;
					if (currentDepth == -1)
						return i;
				}
				else if (currentDepth==0) {
					if (i>begin && formula[i]=='-')
						return i;
					else if (i>begin && formula[i]=='+')
						return i;
				}
			}
			return end;
		}
	}

}
