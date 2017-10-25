using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class SoftJointLimitRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	private 	const	string		kBouncinessKey	= "bounciness";
	private 	const	string		kDamperKey		= "damper";
	private 	const	string		kLimitKey		= "limit";
	private 	const	string		kSpringKey		= "spring";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SoftJointLimit	_jointLimit		= (SoftJointLimit)_object;
		
		// Serialize properties
		_info.AddValue<float>(kBouncinessKey,	_jointLimit.bounciness);
#if UNITY_4_6 || UNITY_4_7
		_info.AddValue<float>(kDamperKey,		_jointLimit.damper);
		_info.AddValue<float>(kLimitKey, 		_jointLimit.limit);
		_info.AddValue<float>(kSpringKey, 		_jointLimit.spring);
#endif

	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SoftJointLimit	_jointLimit		= (SoftJointLimit)_object;

		// Deserialize properties
		_jointLimit.bounciness			= _info.GetValue<float>(kBouncinessKey);
#if UNITY_4_6 || UNITY_4_7
		_jointLimit.damper				= _info.GetValue<float>(kDamperKey);
		_jointLimit.limit				= _info.GetValue<float>(kLimitKey);
		_jointLimit.spring				= _info.GetValue<float>(kSpringKey);
#endif
		
		return _jointLimit;
	}
	
	#endregion
}