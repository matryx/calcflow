using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class BoundsRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	private 	const	string		kCenterKey		= "center";
	private 	const	string		kSizeKey		= "size";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Bounds 	_bounds	= (Bounds)_object;

		// Serialize properties
		_info.AddValue<Vector3>(kCenterKey, _bounds.center);
		_info.AddValue<Vector3>(kSizeKey, 	_bounds.size);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Bounds 	_bounds	= (Bounds)_object;

		// Deserialize properties
		_bounds.center	= _info.GetValue<Vector3>(kCenterKey);
		_bounds.size	= _info.GetValue<Vector3>(kSizeKey);
		
		return _bounds;
	}
	
	#endregion
}