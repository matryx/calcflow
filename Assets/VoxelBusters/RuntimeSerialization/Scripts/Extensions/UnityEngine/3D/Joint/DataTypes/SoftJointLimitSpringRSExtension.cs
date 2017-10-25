using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

#if UNITY_5
public class SoftJointLimitSpringRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	private 	const	string		kDamperKey		= "damper";
	private 	const	string		kSpringKey		= "spring";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SoftJointLimitSpring	_jointLimit		= (SoftJointLimitSpring)_object;
		
		// Serialize properties
		_info.AddValue<float>(kDamperKey,		_jointLimit.damper);
		_info.AddValue<float>(kSpringKey, 		_jointLimit.spring);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SoftJointLimitSpring	_jointLimit		= (SoftJointLimitSpring)_object;

		// Deserialize properties
		_jointLimit.damper						= _info.GetValue<float>(kDamperKey);
		_jointLimit.spring						= _info.GetValue<float>(kSpringKey);
		
		return _jointLimit;
	}
	
	#endregion
}
#endif