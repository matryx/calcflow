using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class SliderJoint2DRSExtension : AnchoredJoint2DRSExtension 
{
	#region Constants
	
	private 	const	string		kAngleKey			= "angle";
	private 	const	string		kLimitsKey			= "limits";
	private 	const	string		kMotorKey			= "motor";
	private 	const	string		kUseLimitsKey		= "useLimits";
	private 	const	string		kUseMotorKey		= "useMotor";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SliderJoint2D	_joint	= _object as SliderJoint2D;
		
		if (_joint == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<float>(kAngleKey, 						_joint.angle);
		_info.AddValue<JointTranslationLimits2D>(kLimitsKey, 	_joint.limits);
		_info.AddValue<JointMotor2D>(kMotorKey, 				_joint.motor);
		_info.AddValue<bool>(kUseLimitsKey, 					_joint.useLimits);
		_info.AddValue<bool>(kUseMotorKey, 						_joint.useMotor);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SliderJoint2D 	_joint	= base.ReadSerializationData(_object, _info) as SliderJoint2D;
		
		if (_joint == null)
			return null;
		
		// Deserialize properties
		_joint.angle			= _info.GetValue<float>(kAngleKey);	
		_joint.limits			= _info.GetValue<JointTranslationLimits2D>(kLimitsKey);	
		_joint.motor			= _info.GetValue<JointMotor2D>(kMotorKey);
		_joint.useLimits		= _info.GetValue<bool>(kUseLimitsKey);
		_joint.useMotor			= _info.GetValue<bool>(kUseMotorKey);
		
		return _joint;
	}
	
	#endregion
}