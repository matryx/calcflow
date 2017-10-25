using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class JointTranslationLimits2DRSExtension : IRuntimeSerializableExtension
{
	#region Constants
	
	private 	const	string		kMaxKey		= "max";
	private 	const	string		kMinKey		= "min";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointTranslationLimits2D	_translationLimits	= (JointTranslationLimits2D)_object;
		
		// Serialize properties
		_info.AddValue<float>(kMaxKey,	_translationLimits.max);
		_info.AddValue<float>(kMinKey,	_translationLimits.min);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointTranslationLimits2D	_translationLimits	= (JointTranslationLimits2D)_object;

		// Deserialize properties
		_translationLimits.max							= _info.GetValue<float>(kMaxKey);
		_translationLimits.min							= _info.GetValue<float>(kMinKey);

		return _translationLimits;
	}
	
	#endregion
}