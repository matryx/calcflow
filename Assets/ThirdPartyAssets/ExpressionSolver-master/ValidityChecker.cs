using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace AK {

	public static class ValidityChecker
	{
		private static Dictionary<char,string> acceptedPreceders = new Dictionary<char, string>()
		{
			{'-',",*()/^0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_π" },
			{'+',",)0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_π" },
			{'/',")0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_π" },
			{'^',")0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_π" },
			{'*',")0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_π" },
			{')',"\')0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_π" },
			{'(',",*-+^/(0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_π" },
			{'\'',",(" }
		};

		private static bool CanPrecede(char l, char r)
		{
			string accepted;
			if (acceptedPreceders.TryGetValue(r,out accepted)) {
				foreach (var c in accepted)
				{
					if (c==l)
					{
						return true;
					}
				}
			}
			return false;
		}

		private static void ThrowSyntaxErrorAt(string expression, int index) {
			int i = index;
			int l = expression.Length;
			int from = System.Math.Max(0,i-3);
			int to = System.Math.Min(l,i+4);
			int len = to-from;
			string str = "Syntax error: ";
			if (from>0) {
				str+="...";
			}
			str += expression.Substring(from,len);
			if (to<l) {
				str+="...";
			}
			throw new ESSyntaxErrorException(str);
		}

		private static bool CanBeBeginOfRValue(char c) {
			if (c >= '0' && c <= '9')
				return true;
			if (c == '(')
				return true;
			if (c == '-')
				return true;
			if (c == '\'')
				return true;
			if (c == '+')
				return true;
			if (c >= 'a' && c <='z')
				return true;
			if (c >= 'A' && c <='Z')
				return true;
			if (c >= '_')
				return true;
			if (c >= '.')
				return true;
			return false;
		}

		public static string CheckNamesAndConstants(string s)
		{
			var rege = @"(?>[_.a-zA-Z0-9]+)"; // ?> forces the regex engine to not backtrack, ?!\( ensures that the word does not end with ( which would make it a function name
			var matches = Regex.Matches(s,rege);
			if (matches.Count>0)
			{
				for (int i=0;i<matches.Count;i++)
				{
					var m = matches[i].Value;
					if (!IsValidNameOrConstant(m))
					{
						return m;
					}
				}
			}
			return null;
		}

		public static bool IsValidNameOrConstant(string s)
		{
			string[] mustMatchWith = {
				@"^[a-zA-Z_][a-zA-Z_0-9]*$" /* a constant, function name - can not start with a number */,
				@"^[0-9]*$",
				@"^[0-9]+.[0-9]*$",
				@"^[0-9]*.[0-9]+$",
			};			
			for (int j=0;j<mustMatchWith.Length;j++)
			{
				if (Regex.IsMatch(s,mustMatchWith[j]))
				{
					return true;
				}
			}
			return false;
		}

		public static void CheckValidity(string expression)
		{
			bool inStringParam = false;
			int parenthesisDepth = 0;
			int l = expression.Length;
			for (int i=0;i<l;i++)
			{
				var x = expression[i];
				if (inStringParam)
				{
					if (x != '\'' || (x == '\'' && expression[i-1] == '\\'))
					{
						continue;
					}
				}
				switch (x) {
					case '(':
						parenthesisDepth++;
						if (i>0 && !CanPrecede(expression[i-1],expression[i])) 
						{
							ThrowSyntaxErrorAt(expression,i);
						}
						break;
					case ')':
						if (parenthesisDepth == 0) {
							throw new ESSyntaxErrorException("Parenthesis mismatch.");
						}
						if (i>0 && !CanPrecede(expression[i-1],expression[i])) {
                            ThrowSyntaxErrorAt(expression,i);
						}
						parenthesisDepth--;
						break;
					case '/':
					case '*':
					case '+':
					case '^':
					case '-':
						if (i==l-1)
							ThrowSyntaxErrorAt(expression,i);
						if (i==0 && !(x=='-' || x=='+') )
							ThrowSyntaxErrorAt(expression,i);
						if (!CanBeBeginOfRValue(expression[i+1])) {
							ThrowSyntaxErrorAt(expression,i);
						}
						if (i>0 && !CanPrecede(expression[i-1],expression[i])) {
							ThrowSyntaxErrorAt(expression,i);
						}
						if ( (x == '+' || x=='-') && i < l-2) {
							if ( (expression[i+2]=='+' || expression[i+2]=='-') && (expression[i+1]=='+' || expression[i+1]=='-') ) {
								ThrowSyntaxErrorAt(expression,i);
							}
						}
						break;
					case ',':
						if (i==l-1)
							ThrowSyntaxErrorAt(expression,i);
						if (!CanBeBeginOfRValue(expression[i+1])) {
							ThrowSyntaxErrorAt(expression,i);
						}
						break;
					case '\'':
						if (!inStringParam)
						{
							if (i == 0 || (i>0 && !CanPrecede(expression[i-1],expression[i])))
							{
								ThrowSyntaxErrorAt(expression,i);
							}
						}
						else
						{
							if (i < l-1 && (expression[i+1] != ')' && expression[i+1] != ','))
							{
								ThrowSyntaxErrorAt(expression,i);
							}
						}
						inStringParam = !inStringParam;
						break;
					case '.':
						if (i==l-1)
							ThrowSyntaxErrorAt(expression,i);
						if (! (expression[i+1] >= '0' && expression[i+1] <= '9') )
							ThrowSyntaxErrorAt(expression,i);
						break;
					default:
						if (x >= '0' && x<= '9')
							break;
						if (x >= 'a' && x<= 'z')
							break;
						if (x >= 'A' && x<= 'Z')
							break;
						if (x == '_')
                            break;
                        if (x == 'π')                                
                            break;
						throw new ESInvalidCharacterException(expression.Substring(i,1));
				}
			}
			if (parenthesisDepth > 0) {
				throw new ESSyntaxErrorException("Parenthesis mismatch.");
			}
			if (inStringParam) {
				throw new ESSyntaxErrorException("String parameter not ending.");
			}
			var error = CheckNamesAndConstants(expression);
			if (error != null)
			{
				throw new ESInvalidNameException(error);
			}
		}

	}
}
