using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class CapsuleColliderRSExtension : ColliderRSExtension 
{
	#region Constants

	private 	const	string		kCenterKey		= "center";
	private 	const	string		kDirectionKey	= "direction";
	private 	const	string		kHeightKey		= "height";
	private 	const	string		kRadiusKey		= "radius";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CapsuleCollider _collider	= _object as CapsuleCollider;
		
		if (_collider == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<Vector3>(kCenterKey, 	_collider.center);
		_info.AddValue<int>(kDirectionKey, 		_collider.direction);
		_info.AddValue<float>(kHeightKey, 		_collider.height);
		_info.AddValue<float>(kRadiusKey, 		_collider.radius);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CapsuleCollider _collider	= base.ReadSerializationData(_object, _info) as CapsuleCollider;
		
		if (_collider == null)
			return null;

		// Deserialize properties
		_collider.center			= _info.GetValue<Vector3>(kCenterKey);
		_collider.direction			= _info.GetValue<int>(kDirectionKey);
		_collider.height			= _info.GetValue<float>(kHeightKey);
		_collider.radius			= _info.GetValue<float>(kRadiusKey);

		return _collider;
	}
	
	#endregion
}