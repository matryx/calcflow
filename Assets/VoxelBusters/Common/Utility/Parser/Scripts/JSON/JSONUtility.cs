using UnityEngine;
using System.Collections;

/// <summary>
/// JSON utility provides interface to encode and decode JSON strings.
/// </summary>
/// <description>
/// Here is how we mapped JSON type to C# type
/// JSON array corresponds to type IList.
/// JSON objects corresponds to type IDictionary.
/// JSON intergers corresponds to type long.
/// JSON real numbers corresponds to type double.
/// Credits: http://techblog.procurios.nl/k/618/news/view/14605/14863/How-do-I-write-my-own-parser-for-JSON.html
/// </description>
namespace VoxelBusters.Utility
{
	using Internal;

	public static class JSONUtility 
	{
		#region Methods
		
		public static string ToJSON (object _object)
		{
			JSONWriter _writer	= new JSONWriter();

			return _writer.Serialise(_object);
		}

		public static bool IsNull (string _jsonStr)
		{
			return _jsonStr.Equals(JSONConstants.kNull);
		}

		public static object FromJSON (string _inputJSONString)
		{
			JSONReader _reader	= new JSONReader(_inputJSONString);

			return _reader.Deserialise();
		}
		
		public static object FromJSON (string _inputJSONString, ref int _errorIndex)
		{
			JSONReader _reader	= new JSONReader(_inputJSONString);
			
			return _reader.Deserialise(ref _errorIndex);
		}

		#endregion
	}
}