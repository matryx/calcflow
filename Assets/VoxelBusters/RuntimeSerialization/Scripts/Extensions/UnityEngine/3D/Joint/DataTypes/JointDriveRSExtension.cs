using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class JointDriveRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	private 	const	string		kMaximumForceKey	= "maxForce";
	private 	const	string		kModeKey			= "mode";
	private 	const	string		kPositionDamperKey	= "posDamper";
	private 	const	string		kPositionSpringKey	= "posSpring";
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointDrive	_jointDrive		= (JointDrive)_object;
		
		// Serialize properties
		_info.AddValue<float>(kMaximumForceKey,	 	_jointDrive.maximumForce);
		_info.AddValue<JointDriveMode>(kModeKey,	_jointDrive.mode);
		_info.AddValue<float>(kPositionDamperKey, 	_jointDrive.positionDamper);
		_info.AddValue<float>(kPositionSpringKey, 	_jointDrive.positionSpring);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		JointDrive	_jointDrive		= (JointDrive)_object;

		// Deserialize properties
		_jointDrive.maximumForce	= _info.GetValue<float>(kMaximumForceKey);
		_jointDrive.mode			= _info.GetValue<JointDriveMode>(kModeKey);
		_jointDrive.positionDamper	= _info.GetValue<float>(kPositionDamperKey);
		_jointDrive.positionSpring	= _info.GetValue<float>(kPositionSpringKey);
		
		return _jointDrive;
	}
	
	#endregion
}