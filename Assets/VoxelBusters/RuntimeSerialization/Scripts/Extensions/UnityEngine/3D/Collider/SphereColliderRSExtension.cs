using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class SphereColliderRSExtension : ColliderRSExtension 
{
	#region Constants

	private 	const	string		kCenterKey		= "center";
	private 	const	string		kRadiusKey		= "radius";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SphereCollider _collider	= _object as SphereCollider;
		
		if (_collider == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<Vector3>(kCenterKey, 	_collider.center);
		_info.AddValue<float>(kRadiusKey, 		_collider.radius);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		SphereCollider _collider	= base.ReadSerializationData(_object, _info) as SphereCollider;
		
		if (_collider == null)
			return null;

		// Deserialize properties
		_collider.center			= _info.GetValue<Vector3>(kCenterKey);
		_collider.radius			= _info.GetValue<float>(kRadiusKey);

		return _collider;
	}
	
	#endregion
}