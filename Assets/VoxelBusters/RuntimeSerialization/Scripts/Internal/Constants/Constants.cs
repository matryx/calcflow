using UnityEngine;
using System.Collections;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal class Constants : MonoBehaviour 
	{
		#region Constants

		// Version section
		internal 	const		string				kProductName							= "Runtime Serialization for Unity";
		internal 	const		string				kProductVersion							= "1.2.1";
		internal	const		int					kSerializationFormatVersion				= 1;
		internal	const		int					kSerializationSupportedFormatVersions	= kSerializationFormatVersion;

		// RSExtension's section
		internal	const		string				kExtensionSupportSuffix					= "RSExtension";

		#endregion
	}
}