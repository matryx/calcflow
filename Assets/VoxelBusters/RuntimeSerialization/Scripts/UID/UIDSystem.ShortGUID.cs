using UnityEngine;
using System.Collections;
using System;

namespace VoxelBusters.RuntimeSerialization
{
	public partial class UIDSystem : MonoBehaviour 
	{
		public class ShortGUID 
		{
			#region Static Methods

			public static string Create()
			{
				string _encodedStr 	= Convert.ToBase64String(Guid.NewGuid().ToByteArray());

				// To make it hierarchy path safe variant, character 63 "/" is replaced with a "_" (underscore).
				// Ref: http://www.garykessler.net/library/base64.html
				_encodedStr			= _encodedStr.Replace('/', '_');

				// Excluding == from encoded string
				return _encodedStr.Substring(0, 22);
			}

			#endregion
		}
	}
}