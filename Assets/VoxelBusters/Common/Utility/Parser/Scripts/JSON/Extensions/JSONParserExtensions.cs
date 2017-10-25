using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public static class JSONParserExtensions 
	{
		#region Generic Type Extension Methods

		public static string ToJSON (this IDictionary _dictionary)
		{
			string _jsonStr	= JSONUtility.ToJSON(_dictionary);
			
			return JSONUtility.IsNull(_jsonStr) ? null : _jsonStr;
		}

		public static string ToJSON (this IList _list)
		{
			string _jsonStr	=  JSONUtility.ToJSON(_list);
			
			return JSONUtility.IsNull(_jsonStr) ? null : _jsonStr;
		}

		#endregion
	}
}