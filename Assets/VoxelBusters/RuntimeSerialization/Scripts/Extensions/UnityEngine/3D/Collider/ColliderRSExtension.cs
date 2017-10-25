using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class ColliderRSExtension : ComponentRSExtension 
{
	#region Constants
	
	private 	const	string		kEnabledKey			= "enabled";
	private 	const	string		kIsTriggerKey		= "isTrigger";
	private 	const	string		kMaterialKey		= "material";
	private 	const	string		kSharedMaterialKey	= "sharedMaterial";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Collider 		_collider	= _object as Collider;
		
		if (_collider == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<bool>(kEnabledKey, 						_collider.enabled);
		_info.AddValue<bool>(kIsTriggerKey, 					_collider.isTrigger);

		PhysicMaterial 	_sharedMat	= _collider.sharedMaterial;

		if (_sharedMat == null)
			_info.AddValue<PhysicMaterial>(kMaterialKey, 		_collider.material);
		else
			_info.AddValue<PhysicMaterial>(kSharedMaterialKey, 	_sharedMat);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Collider 		_collider	= base.ReadSerializationData(_object, _info) as Collider;
		
		if (_collider == null)
			return null;
		
		// Deserialize properties
		_collider.enabled			= _info.GetValue<bool>(kEnabledKey);
		_collider.isTrigger			= _info.GetValue<bool>(kIsTriggerKey);

		PhysicMaterial 	_sharedMat	= _info.GetValue<PhysicMaterial>(kSharedMaterialKey);

		if (_sharedMat == null)
			_collider.material		= _info.GetValue<PhysicMaterial>(kMaterialKey);
		else
			_collider.sharedMaterial= _sharedMat;

		return _collider;
	}
	
	#endregion
}