using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class JointMotor2DRSExtension : IRuntimeSerializableExtension
{
	#region Constants
		
	private 	const	string		kMaxMotorTorqueKey		= "maxMotorTorque";
	private 	const	string		kMotorSpeedKey			= "motorSpeed";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointMotor2D	_jointMotor	= (JointMotor2D)_object;
		
		// Serialize properties
		_info.AddValue<float>(kMaxMotorTorqueKey,	_jointMotor.maxMotorTorque);
		_info.AddValue<float>(kMotorSpeedKey,		_jointMotor.motorSpeed);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointMotor2D	_jointMotor	= (JointMotor2D)_object;
		
		// Deserialize properties
		_jointMotor.maxMotorTorque	= _info.GetValue<float>(kMaxMotorTorqueKey);
		_jointMotor.motorSpeed		= _info.GetValue<float>(kMotorSpeedKey);

		return _jointMotor;
	}
	
	#endregion
}