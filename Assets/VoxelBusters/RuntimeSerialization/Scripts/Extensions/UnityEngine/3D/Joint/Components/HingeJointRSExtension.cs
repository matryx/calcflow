using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class HingeJointRSExtension : JointRSExtension 
{
	#region Constants
	
	private 	const	string		kLimitsKey			= "limits";
	private 	const	string		kMotorKey			= "motor";
	private 	const	string		kSpringKey			= "spring";
	private 	const	string		kUseLimitsKey		= "useLimits";
	private 	const	string		kUseMotorKey		= "useMotor";
	private 	const	string		kUseSpringKey		= "useSpring";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		HingeJoint	_joint		= _object as HingeJoint;
		
		if (_joint == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<JointLimits>(kLimitsKey,		_joint.limits);
		_info.AddValue<JointMotor>(kMotorKey,		_joint.motor);
		_info.AddValue<JointSpring>(kSpringKey,		_joint.spring);
		_info.AddValue<bool>(kUseLimitsKey,			_joint.useLimits);
		_info.AddValue<bool>(kUseMotorKey,			_joint.useMotor);
		_info.AddValue<bool>(kUseSpringKey,			_joint.useSpring);
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		HingeJoint	_joint		= base.ReadSerializationData(_object, _info) as HingeJoint;
		
		if (_joint == null)
			return null;
		
		// Deserialize properties
		_joint.limits			= _info.GetValue<JointLimits>(kLimitsKey);
		_joint.motor			= _info.GetValue<JointMotor>(kMotorKey);
		_joint.spring			= _info.GetValue<JointSpring>(kSpringKey);	
		_joint.useLimits		= _info.GetValue<bool>(kUseLimitsKey);
		_joint.useMotor			= _info.GetValue<bool>(kUseMotorKey);	
		_joint.useSpring		= _info.GetValue<bool>(kUseSpringKey);
		
		return _joint;
	}

	#endregion
}