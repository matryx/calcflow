using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility.Internal
{
	internal class JSONConstants : MonoBehaviour 
	{
		internal const 	string 		kNull				= "null";
		internal const 	string 		kBoolTrue			= "true";
		internal const 	string 		kBoolFalse			= "false";
		internal const 	string		kWhiteSpaceLiterals	= " \n\t\r";
		internal const	string		kNumericLiterals	= "0123456789+-.eE";
	}
}