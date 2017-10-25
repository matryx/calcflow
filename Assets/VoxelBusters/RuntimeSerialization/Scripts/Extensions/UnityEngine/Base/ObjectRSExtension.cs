using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

namespace UnityEngine
{
	public class ObjectRSExtension : IRuntimeSerializableExtension 
	{
		#region Constants
		
		private 	const	string		kHideFlagsKey	= "hideFlags";

		#endregion
		
		#region Serialization Methods
		
		public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
		{
			UnityEngine.Object _unityObject	= _object as UnityEngine.Object;
			
			if (_unityObject == null)
				return;

			// Serialize properties
			_info.AddValue<HideFlags>(kHideFlagsKey, _unityObject.hideFlags);
		}
		
		public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
		{
			UnityEngine.Object _unityObject	= _object as UnityEngine.Object;

			if (_unityObject == null)
				return null;
			
			// Deserialize properties
			_unityObject.hideFlags			= _info.GetValue<HideFlags>(kHideFlagsKey);

			return _unityObject;
		}
		
		#endregion
	}
}