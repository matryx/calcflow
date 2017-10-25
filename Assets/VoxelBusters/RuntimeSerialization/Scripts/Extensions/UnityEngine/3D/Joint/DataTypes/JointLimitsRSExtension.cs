using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class JointLimitsRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	private 	const	string		kMaxKey			= "max";
	private 	const	string		kMaxBounceKey	= "maxBounce";
	private 	const	string		kMinKey			= "min";
	private 	const	string		kMinBounceKey	= "minBounce";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointLimits	_jointLimits	= (JointLimits)_object;
		
		// Serialize properties
		_info.AddValue<float>(kMaxKey,	 		_jointLimits.max);
		_info.AddValue<float>(kMinKey, 			_jointLimits.min);
		
#if UNITY_5 && !UNITY_5_0
		_info.AddValue<float>(kMaxBounceKey,	_jointLimits.bounciness);
		_info.AddValue<float>(kMinBounceKey,	_jointLimits.bounciness);
#else
		_info.AddValue<float>(kMaxBounceKey,	_jointLimits.maxBounce);
		_info.AddValue<float>(kMinBounceKey,	_jointLimits.minBounce);
#endif

	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointLimits	_jointLimits	= (JointLimits)_object;

		// Deserialize properties
		_jointLimits.max			= _info.GetValue<float>(kMaxKey);
		_jointLimits.min			= _info.GetValue<float>(kMinKey);

#if UNITY_5 && !UNITY_5_0
		_jointLimits.bounciness		=  _info.GetValue<float>(kMaxBounceKey);
#else
		_jointLimits.maxBounce		= _info.GetValue<float>(kMaxBounceKey);
		_jointLimits.minBounce		= _info.GetValue<float>(kMinBounceKey);
#endif
		
		return _jointLimits;
	}
	
	#endregion
}