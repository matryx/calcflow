using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class WheelJoint2DRSExtension : AnchoredJoint2DRSExtension 
{
	#region Constants
	
	private 	const	string		kMotorKey				= "motor";
	private 	const	string		kSuspensionKey			= "suspension";
	private 	const	string		kUseMotorKey			= "useMotor";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		WheelJoint2D	_joint		= _object as WheelJoint2D;
		
		if (_joint == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serializing properties
		_info.AddValue<JointMotor2D>(kMotorKey, 			_joint.motor);
		_info.AddValue<JointSuspension2D>(kSuspensionKey, 	_joint.suspension);
		_info.AddValue<bool>(kUseMotorKey, 					_joint.useMotor);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		WheelJoint2D 	_joint		= base.ReadSerializationData(_object, _info) as WheelJoint2D;
		
		if (_joint == null)
			return null;
		
		// Deserialize properties
		_joint.motor				= _info.GetValue<JointMotor2D>(kMotorKey);
		_joint.suspension			= _info.GetValue<JointSuspension2D>(kSuspensionKey);
		_joint.useMotor				= _info.GetValue<bool>(kUseMotorKey);
		
		return _joint;
	}
	
	#endregion
}