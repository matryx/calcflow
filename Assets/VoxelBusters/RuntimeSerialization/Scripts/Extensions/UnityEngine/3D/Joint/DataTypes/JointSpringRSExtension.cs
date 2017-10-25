using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class JointSpringRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	private 	const	string		kDamperKey			= "damper";
	private 	const	string		kSpringKey			= "spring";
	private 	const	string		kTargetPositionKey	= "targetPosition";
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointSpring	_jointSpring	= (JointSpring)_object;
		
		// Serialize properties
		_info.AddValue<float>(kDamperKey,	 		_jointSpring.damper);
		_info.AddValue<float>(kSpringKey,			_jointSpring.spring);
		_info.AddValue<float>(kTargetPositionKey, 	_jointSpring.targetPosition);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointSpring	_jointSpring	= (JointSpring)_object;

		// Deserialize properties
		_jointSpring.damper			= _info.GetValue<float>(kDamperKey);
		_jointSpring.spring			= _info.GetValue<float>(kSpringKey);
		_jointSpring.targetPosition	= _info.GetValue<float>(kTargetPositionKey);
		
		return _jointSpring;
	}
	
	#endregion
}