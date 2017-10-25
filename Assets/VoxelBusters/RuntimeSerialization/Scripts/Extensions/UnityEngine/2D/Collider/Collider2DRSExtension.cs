using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class Collider2DRSExtension : BehaviourRSExtension 
{
	#region Constants
	
	private 	const	string		kIsTriggerKey		= "isTrigger";
	private 	const	string		kSharedMaterialKey	= "sharedMaterial";

#if UNITY_5
	private 	const	string		kOffsetKey			= "offset";
	private 	const	string		kUsedByEffectorKey	= "usedByEffector";
#endif

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Collider2D		_collider	= _object as Collider2D;
		
		if (_collider == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<bool>(kIsTriggerKey, 					_collider.isTrigger);
		_info.AddValue<PhysicsMaterial2D>(kSharedMaterialKey, 	_collider.sharedMaterial);

#if UNITY_5
		_info.AddValue<Vector2>(kOffsetKey, 					_collider.offset);
		_info.AddValue<bool>(kUsedByEffectorKey, 				_collider.usedByEffector);
#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Collider2D 		_collider	= base.ReadSerializationData(_object, _info) as Collider2D;
		
		if (_collider == null)
			return null;
		
		// Deserialize properties
		_collider.isTrigger			= _info.GetValue<bool>(kIsTriggerKey);
		_collider.sharedMaterial	= _info.GetValue<PhysicsMaterial2D>(kSharedMaterialKey);

#if UNITY_5
		_collider.offset			= _info.GetValue<Vector2>(kOffsetKey);
		_collider.usedByEffector	= _info.GetValue<bool>(kUsedByEffectorKey);
#endif

		return _collider;
	}
	
	#endregion
}