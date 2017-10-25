using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class PhysicsMaterial2DRSExtension : ObjectRSExtension 
{
	#region Constants
	
	private 	const		string		kBouncinessKey					= "bounciness";
	private 	const		string		kFrictionKey					= "friction";

	#endregion

	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		return new PhysicsMaterial2D();
	}
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		PhysicsMaterial2D _material		= _object as PhysicsMaterial2D;
		
		if (_material == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<float>(kBouncinessKey, 	_material.bounciness);
		_info.AddValue<float>(kFrictionKey, 	_material.friction);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		PhysicsMaterial2D _material		= base.ReadSerializationData(_object, _info) as PhysicsMaterial2D;

		if (_material == null)
			return null;
		
		// Deserialize properties
		_material.bounciness			= _info.GetValue<float>(kBouncinessKey);
		_material.friction				= _info.GetValue<float>(kFrictionKey);
		
		return _material;
	}
	
	#endregion
}