using UnityEngine;
using System.Collections;
using System.Text;

namespace VoxelBusters.Utility
{
	public static class StringExtensions
	{
		#region String Operations

		public static string GetPrintableString (this string _string)
		{
			return _string == null ? "NULL" : _string;
		}

		public static bool Contains (this string _string, string _stringToCheck, bool _ignoreCase)
		{
			if(!_ignoreCase)
			{
				return _string.Contains(_stringToCheck);
			}
			else
			{
				return _string.ToLower().Contains(_stringToCheck.ToLower());
			}
		}

		public static string ToBase64(this string _string)
		{
			byte[] _bytesToEncode 	= Encoding.UTF8.GetBytes (_string);
			string _encoded			= System.Convert.ToBase64String (_bytesToEncode);
			return _encoded;
		}

		public static string FromBase64(this string _string)
		{
			byte[] _bytesEncodedInBase64 = System.Convert.FromBase64String(_string);
			string _decoded	 = System.Text.Encoding.UTF8.GetString(_bytesEncodedInBase64, 0, _bytesEncodedInBase64.Length);
			return _decoded;
		}

		//Returns string between _startString and _endString from _string.
		public static string StringBetween(this string _string, string _startString, string _endString, bool _ignoreCase)
		{
			System.StringComparison _comparision 	=	_ignoreCase ? System.StringComparison.OrdinalIgnoreCase : System.StringComparison.Ordinal;

			int _startStringLength 	= _startString 	!= null ? _startString.Length 	: 0;

			

			int _startStringOccuranceIndex 			=	_string.IndexOf(_startString, _comparision);
		
			//Check the end string next to the occurance of above start string.
			int _endStringOccuranceIndex			=	_string.IndexOf(_endString, _startStringOccuranceIndex + _startStringLength, _comparision);


			string _subString;

			if(_startStringOccuranceIndex == -1 || _endStringOccuranceIndex == -1)
			{
				_subString = "";
			}
			else
			{
				int  _lengthRequired = _endStringOccuranceIndex - (_startStringOccuranceIndex + _startStringLength);//Shouldn't include the strings use for matching
				_subString	=	_string.Substring(_startStringOccuranceIndex + _startStringLength, _lengthRequired);
			}
			
			return _subString;
		}

		#endregion
	}
}