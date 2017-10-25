using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class JointSuspension2DRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
		
	private 	const	string		kAngleKey			= "angle";
	private 	const	string		kDampingRatioKey	= "dampingRatio";
	private 	const	string		kFrequencyKey		= "frequency";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointSuspension2D	_jointSuspension	= (JointSuspension2D)_object;
		
		// Serialize properties
		_info.AddValue<float>(kAngleKey,	 		_jointSuspension.angle);
		_info.AddValue<float>(kDampingRatioKey,		_jointSuspension.dampingRatio);
		_info.AddValue<float>(kFrequencyKey, 		_jointSuspension.frequency);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointSuspension2D	_jointSuspension	= (JointSuspension2D)_object;

		// Deserialize properties
		_jointSuspension.angle					= _info.GetValue<float>(kAngleKey);
		_jointSuspension.dampingRatio			= _info.GetValue<float>(kDampingRatioKey);
		_jointSuspension.frequency				= _info.GetValue<float>(kFrequencyKey);
		
		return _jointSuspension;
	}
	
	#endregion
}