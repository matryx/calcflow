using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class JointAngleLimits2DRSExtension : IRuntimeSerializableExtension
{
	#region Constants
	
	private 	const	string		kMaxKey		= "max";
	private 	const	string		kMinKey		= "min";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointAngleLimits2D	_angleLimits	= (JointAngleLimits2D)_object;
		
		// Serialize properties
		_info.AddValue<float>(kMaxKey,	_angleLimits.max);
		_info.AddValue<float>(kMinKey,	_angleLimits.min);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointAngleLimits2D	_angleLimits	= (JointAngleLimits2D)_object;

		// Deserialize properties
		_angleLimits.max					= _info.GetValue<float>(kMaxKey);
		_angleLimits.min					= _info.GetValue<float>(kMinKey);
		
		return _angleLimits;
	}
	
	#endregion
}