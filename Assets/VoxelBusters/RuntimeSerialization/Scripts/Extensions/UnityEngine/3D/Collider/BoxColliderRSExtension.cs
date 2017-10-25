using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class BoxColliderRSExtension : ColliderRSExtension 
{
	#region Constants

	private 	const	string		kCenterKey		= "center";
	private 	const	string		kSizeKey		= "size";
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		BoxCollider _collider	= _object as BoxCollider;
		
		if (_collider == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<Vector3>(kCenterKey, 	_collider.center);
		_info.AddValue<Vector3>(kSizeKey, 		_collider.size);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		BoxCollider _collider	= base.ReadSerializationData(_object, _info) as BoxCollider;
		
		if (_collider == null)
			return null;

		// Deserialize properties
		_collider.center		= _info.GetValue<Vector3>(kCenterKey);
		_collider.size			= _info.GetValue<Vector3>(kSizeKey);

		return _collider;
	}
	
	#endregion
}