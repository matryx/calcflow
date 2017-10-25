using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class BoxCollider2DRSExtension : Collider2DRSExtension 
{
	#region Constants
	
	private 	const	string		kSizeKey		= "size";

#if UNITY_4_6 || UNITY_4_7
	private 	const	string		kCenterKey		= "center";
#endif

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		BoxCollider2D	_collider	= _object as BoxCollider2D;
		
		if (_collider == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<Vector2>(kSizeKey, 	_collider.size);

#if UNITY_4_6 || UNITY_4_7
		_info.AddValue<Vector2>(kCenterKey, _collider.center);
#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		BoxCollider2D 	_collider	= base.ReadSerializationData(_object, _info) as BoxCollider2D;
		
		if (_collider == null)
			return null;
		
		// Deserialize properties
		_collider.size				= _info.GetValue<Vector2>(kSizeKey);

#if UNITY_4_6 || UNITY_4_7
		_collider.center			= _info.GetValue<Vector2>(kCenterKey);
#endif

		return _collider;
	}
	
	#endregion
}
