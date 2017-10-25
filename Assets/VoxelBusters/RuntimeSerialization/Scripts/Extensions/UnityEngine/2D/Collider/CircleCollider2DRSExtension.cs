using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class CircleCollider2DRSExtension : Collider2DRSExtension 
{
	#region Constants
	
	private 	const	string		kRadiusKey		= "radius";
	
#if UNITY_4_6 || UNITY_4_7
	private 	const	string		kCenterKey		= "center";
#endif

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CircleCollider2D	_collider	= _object as CircleCollider2D;
		
		if (_collider == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<float>(kRadiusKey, 	_collider.radius);
	
#if UNITY_4_6 || UNITY_4_7
		_info.AddValue<Vector2>(kCenterKey, _collider.center);
#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CircleCollider2D 	_collider	= base.ReadSerializationData(_object, _info) as CircleCollider2D;
		
		if (_collider == null)
			return null;
		
		// Deserialize properties
		_collider.radius				= _info.GetValue<float>(kRadiusKey);

#if UNITY_4_6 || UNITY_4_7
		_collider.center				= _info.GetValue<Vector2>(kCenterKey);
#endif

		return _collider;
	}
	
	#endregion
}
