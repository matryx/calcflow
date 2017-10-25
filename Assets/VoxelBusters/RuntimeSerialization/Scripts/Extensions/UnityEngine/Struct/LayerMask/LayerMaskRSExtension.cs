using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class LayerMaskRSExtension : IRuntimeSerializableExtension
{
	#region Constants
	
	private 	const	string		kValueKey	= "value";

	#endregion

	#region Instance Method

	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		return (LayerMask)_info.GetValue<int>(kValueKey, true);
	}

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		LayerMask	_layerMask	= (LayerMask)_object;
		
		// Serialize properties
		_info.AddValue<int>(kValueKey, _layerMask.value, true);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		return _object;
	}
	
	#endregion
}