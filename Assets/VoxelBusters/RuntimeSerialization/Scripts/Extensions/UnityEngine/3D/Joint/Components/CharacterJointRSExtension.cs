using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class CharacterJointRSExtension : JointRSExtension 
{
	#region Constants
	
	private 	const	string		kHighTwistLimitKey			= "highTwistLimit";
	private 	const	string		kLowTwistLimitKey			= "lowTwistLimit";
	private 	const	string		kSwing1LimitKey				= "swing1Limit";
	private 	const	string		kSwing2LimitKey				= "swing2Limit";
	private 	const	string		kSwingAxisKey				= "swingAxis";

#if UNITY_4_6 || UNITY_4_7
	private 	const	string		kRotationDriveKey			= "rotationDrive";
	private 	const	string		kTargetAngularVelocityKey	= "targetAngularVelocity";
	private 	const	string		kTargetRotationKey			= "targetRotation";
#elif UNITY_5
	private 	const	string		kEnableProjection			= "enableProjection";
	private 	const	string		kProjectionAngleKey			= "projectionAngle";
	private 	const	string		kProjectionDistanceKey		= "projectionDistance";
	private 	const	string		kSwingLimitSpringKey		= "swingLimitSpring";
	private 	const	string		kTwistLimitSpringKey		= "twistLimitSpring";
#endif

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CharacterJoint		_joint		= _object as CharacterJoint;

		if (_joint == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<SoftJointLimit>(kHighTwistLimitKey,	_joint.highTwistLimit);
		_info.AddValue<SoftJointLimit>(kLowTwistLimitKey,	_joint.lowTwistLimit);
		_info.AddValue<SoftJointLimit>(kSwing1LimitKey,		_joint.swing1Limit);
		_info.AddValue<SoftJointLimit>(kSwing2LimitKey,	 	_joint.swing2Limit);
		_info.AddValue<Vector3>(kSwingAxisKey,	 			_joint.swingAxis);

#if UNITY_4_6 || UNITY_4_7
		_info.AddValue<JointDrive>(kRotationDriveKey,		_joint.rotationDrive);
		_info.AddValue<Vector3>(kTargetAngularVelocityKey,	_joint.targetAngularVelocity);
		_info.AddValue<Quaternion>(kTargetRotationKey,	 	_joint.targetRotation);
#elif UNITY_5
		_info.AddValue<bool>(kEnableProjection,	 					_joint.enableProjection);
		_info.AddValue<float>(kProjectionAngleKey,	 				_joint.projectionAngle);
		_info.AddValue<float>(kProjectionDistanceKey,	 			_joint.projectionDistance);
		_info.AddValue<SoftJointLimitSpring>(kSwingLimitSpringKey,	_joint.swingLimitSpring);
		_info.AddValue<SoftJointLimitSpring>(kTwistLimitSpringKey,	_joint.twistLimitSpring);
#endif
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CharacterJoint		_joint		= base.ReadSerializationData(_object, _info) as CharacterJoint;

		if (_joint == null)
			return null;
		
		// Deserialize properties
		_joint.highTwistLimit			= _info.GetValue<SoftJointLimit>(kHighTwistLimitKey);
		_joint.lowTwistLimit			= _info.GetValue<SoftJointLimit>(kLowTwistLimitKey);
		_joint.swing1Limit				= _info.GetValue<SoftJointLimit>(kSwing1LimitKey);
		_joint.swing2Limit				= _info.GetValue<SoftJointLimit>(kSwing2LimitKey);
		_joint.swingAxis				= _info.GetValue<Vector3>(kSwingAxisKey);

#if UNITY_4_6 || UNITY_4_7
		_joint.rotationDrive			= _info.GetValue<JointDrive>(kRotationDriveKey);
		_joint.targetAngularVelocity	= _info.GetValue<Vector3>(kTargetAngularVelocityKey);
		_joint.targetRotation			= _info.GetValue<Quaternion>(kTargetRotationKey);
#elif UNITY_5
		_joint.enableProjection			= _info.GetValue<bool>(kEnableProjection);
		_joint.projectionAngle			= _info.GetValue<float>(kProjectionAngleKey);
		_joint.projectionDistance		= _info.GetValue<float>(kProjectionDistanceKey);
		_joint.swingLimitSpring			= _info.GetValue<SoftJointLimitSpring>(kSwingLimitSpringKey);
		_joint.twistLimitSpring			= _info.GetValue<SoftJointLimitSpring>(kTwistLimitSpringKey);
#endif

		return _joint;
	}

	#endregion
}