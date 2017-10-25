using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class JointMotorRSExtension : IRuntimeSerializableExtension 
{
	#region Constants

	private 	const	string		kForceKey			= "force";
	private 	const	string		kFreeSpinKey		= "freeSpin";
	private 	const	string		kTargetVelocityKey	= "targetVelocity";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointMotor	_jointMotor		= (JointMotor)_object;
		
		// Serialize properties
		_info.AddValue<float>(kForceKey,	 		_jointMotor.force);
		_info.AddValue<bool>(kFreeSpinKey,			_jointMotor.freeSpin);
		_info.AddValue<float>(kTargetVelocityKey, 	_jointMotor.targetVelocity);
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointMotor	_jointMotor		= (JointMotor)_object;
		
		// Deserialize properties
		_jointMotor.force			= _info.GetValue<float>(kForceKey);
		_jointMotor.freeSpin		= _info.GetValue<bool>(kFreeSpinKey);
		_jointMotor.targetVelocity	= _info.GetValue<float>(kTargetVelocityKey);

		return _jointMotor;
	}

	#endregion
}