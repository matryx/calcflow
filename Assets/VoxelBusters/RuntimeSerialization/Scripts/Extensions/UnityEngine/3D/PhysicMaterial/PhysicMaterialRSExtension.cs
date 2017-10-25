using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class PhysicMaterialRSExtension : ObjectRSExtension
{
	#region Constants

	private 	const		string		kBounceCombineKey					= "bounceCombine";
	private 	const		string		kBouncinessKey						= "bounciness";
	private 	const		string		kDynamicFrictionKey					= "dynamicFriction";
	private 	const		string		kFrictionCombineKey					= "frictionCombine";
	private 	const		string		kStaticFrictionKey					= "staticFriction";

#if UNITY_4_6
	private 	const		string		kDynamicFriction2Key				= "dynamicFriction2";
	private 	const		string		kFrictionDirection2Key				= "frictionDirection2";
	private 	const		string		kStaticFriction2Key					= "staticFriction2";
#endif

	#endregion

	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		return new PhysicMaterial();
	}
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		PhysicMaterial 	_material		= _object as PhysicMaterial;
		
		if (_material == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<PhysicMaterialCombine>(kBounceCombineKey, 	_material.bounceCombine);
		_info.AddValue<float>(kBouncinessKey, 						_material.bounciness);
		_info.AddValue<float>(kDynamicFrictionKey, 					_material.dynamicFriction);
		_info.AddValue<PhysicMaterialCombine>(kFrictionCombineKey,	_material.frictionCombine);
		_info.AddValue<float>(kStaticFrictionKey, 					_material.staticFriction);

#if UNITY_4_6
		_info.AddValue<float>(kDynamicFriction2Key,	 				_material.dynamicFriction2);
		_info.AddValue<Vector3>(kFrictionDirection2Key, 			_material.frictionDirection2);
		_info.AddValue<float>(kStaticFriction2Key, 					_material.staticFriction2);
#endif
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		PhysicMaterial 	_material		= base.ReadSerializationData(_object, _info) as PhysicMaterial;

		if (_material == null)
			return null;

		// Deserialize properties
		_material.bounceCombine			= _info.GetValue<PhysicMaterialCombine>(kBounceCombineKey);
		_material.bounciness			= _info.GetValue<float>(kBouncinessKey);
		_material.dynamicFriction		= _info.GetValue<float>(kDynamicFrictionKey);
		_material.frictionCombine		= _info.GetValue<PhysicMaterialCombine>(kFrictionCombineKey);
		_material.staticFriction		= _info.GetValue<float>(kStaticFrictionKey);

#if UNITY_4_6
		_material.dynamicFriction2		= _info.GetValue<float>(kDynamicFriction2Key);
		_material.frictionDirection2	= _info.GetValue<Vector3>(kFrictionDirection2Key);
		_material.staticFriction2		= _info.GetValue<float>(kStaticFriction2Key);
#endif

		return _material;
	}

	#endregion
}		