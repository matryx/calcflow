using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class ConfigurableJointRSExtension : JointRSExtension 
{
	#region Constants
	
	private 	const	string		kAngularXDriveKey			= "angularXDrive";
	private 	const	string		kAngularXMotionKey			= "angularXMotion";
	private 	const	string		kAngularYLimitKey			= "angularYLimit";
	private 	const	string		kAngularYMotionKey			= "angularYMotion";
	private 	const	string		kAngularYZDriveKey			= "angularYZDrive";
	private 	const	string		kAngularZLimitKey			= "angularZLimit";
	private 	const	string		kAngularZMotionKey			= "angularZMotion";
	private 	const	string		kConfiguredInWorldSpaceKey	= "configuredInWorldSpace";
	private 	const	string		kHighAngularXLimitKey		= "highAngularXLimit";
	private 	const	string		kLinearLimitKey				= "linearLimit";
	private 	const	string		kLowAngularXLimitKey		= "lowAngularXLimit";
	private 	const	string		kProjectionAngleKey			= "projectionAngle";
	private 	const	string		kProjectionDistanceKey		= "projectionDistance";
	private 	const	string		kProjectionModeKey			= "projectionMode";
	private 	const	string		kRotationDriveModeKey		= "rotationDriveMode";
	private 	const	string		kSecondaryAxisKey			= "secondaryAxis";
	private 	const	string		kSlerpDriveKey				= "slerpDrive";
	private 	const	string		kSwapBodiesKey				= "swapBodies";
	private 	const	string		kTargetAngularVelocityKey	= "targetAngularVelocity";
	private 	const	string		kTargetPositionKey			= "targetPosition";
	private 	const	string		kTargetRotationKey			= "targetRotation";
	private 	const	string		kTargetVelocityKey			= "targetVelocity";
	private 	const	string		kXDriveKey					= "xDrive";
	private 	const	string		kXMotionKey					= "xMotion";
	private 	const	string		kYDriveKey					= "yDrive";
	private 	const	string		kYMotionKey					= "yMotion";
	private 	const	string		kZDriveKey					= "zDrive";
	private 	const	string		kZMotionKey					= "zMotion";

#if UNITY_5
	private 	const	string		kAngularXLimitSpringKey		= "angularXLimitSpring";
	private 	const	string		kAngularYZLimitSpringKey	= "angularYZLimitSpring";
	private 	const	string		kLinearLimitSpringKey		= "linearLimitSpring";
#endif

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		ConfigurableJoint	_joint		= _object as ConfigurableJoint;

		if (_joint == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<JointDrive>(kAngularXDriveKey,						_joint.angularXDrive);
		_info.AddValue<ConfigurableJointMotion>(kAngularXMotionKey,			_joint.angularXMotion);
		_info.AddValue<SoftJointLimit>(kAngularYLimitKey,					_joint.angularYLimit);
		_info.AddValue<ConfigurableJointMotion>(kAngularYMotionKey,	 		_joint.angularYMotion);
		_info.AddValue<JointDrive>(kAngularYZDriveKey,	 					_joint.angularYZDrive);
		_info.AddValue<SoftJointLimit>(kAngularZLimitKey,					_joint.angularZLimit);
		_info.AddValue<ConfigurableJointMotion>(kAngularZMotionKey,			_joint.angularZMotion);
		_info.AddValue<bool>(kConfiguredInWorldSpaceKey,					_joint.configuredInWorldSpace);
		_info.AddValue<SoftJointLimit>(kHighAngularXLimitKey,				_joint.highAngularXLimit);
		_info.AddValue<SoftJointLimit>(kLinearLimitKey,						_joint.linearLimit);
		_info.AddValue<SoftJointLimit>(kLowAngularXLimitKey,				_joint.lowAngularXLimit);
		_info.AddValue<float>(kProjectionAngleKey,							_joint.projectionAngle);
		_info.AddValue<float>(kProjectionDistanceKey,						_joint.projectionDistance);
		_info.AddValue<JointProjectionMode>(kProjectionModeKey,				_joint.projectionMode);
		_info.AddValue<RotationDriveMode>(kRotationDriveModeKey,			_joint.rotationDriveMode);
		_info.AddValue<Vector3>(kSecondaryAxisKey,							_joint.secondaryAxis);
		_info.AddValue<JointDrive>(kSlerpDriveKey,							_joint.slerpDrive);
		_info.AddValue<bool>(kSwapBodiesKey,								_joint.swapBodies);
		_info.AddValue<Vector3>(kTargetAngularVelocityKey,					_joint.targetAngularVelocity);
		_info.AddValue<Vector3>(kTargetPositionKey,							_joint.targetPosition);
		_info.AddValue<Quaternion>(kTargetRotationKey,						_joint.targetRotation);
		_info.AddValue<Vector3>(kTargetVelocityKey,							_joint.targetVelocity);
		_info.AddValue<JointDrive>(kXDriveKey,								_joint.xDrive);
		_info.AddValue<ConfigurableJointMotion>(kXMotionKey,				_joint.xMotion);
		_info.AddValue<JointDrive>(kYDriveKey,								_joint.yDrive);
		_info.AddValue<ConfigurableJointMotion>(kYMotionKey,				_joint.yMotion);
		_info.AddValue<JointDrive>(kZDriveKey,								_joint.zDrive);
		_info.AddValue<ConfigurableJointMotion>(kZMotionKey,				_joint.zMotion);

#if UNITY_5
		_info.AddValue<SoftJointLimitSpring>(kAngularXLimitSpringKey,	 	_joint.angularXLimitSpring);
		_info.AddValue<SoftJointLimitSpring>(kAngularYZLimitSpringKey,	 	_joint.angularYZLimitSpring);
		_info.AddValue<SoftJointLimitSpring>(kLinearLimitSpringKey,	 		_joint.linearLimitSpring);
#endif
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		ConfigurableJoint	_joint		= base.ReadSerializationData(_object, _info) as ConfigurableJoint;

		if (_joint == null)
			return null;
		
		// Deserialize properties
		_joint.angularXDrive			= _info.GetValue<JointDrive>(kAngularXDriveKey);
		_joint.angularXMotion			= _info.GetValue<ConfigurableJointMotion>(kAngularXMotionKey);
		_joint.angularYLimit			= _info.GetValue<SoftJointLimit>(kAngularYLimitKey);
		_joint.angularYMotion			= _info.GetValue<ConfigurableJointMotion>(kAngularYMotionKey);
		_joint.angularYZDrive			= _info.GetValue<JointDrive>(kAngularYZDriveKey);
		_joint.angularZLimit			= _info.GetValue<SoftJointLimit>(kAngularZLimitKey);
		_joint.angularZMotion			= _info.GetValue<ConfigurableJointMotion>(kAngularZMotionKey);
		_joint.configuredInWorldSpace	= _info.GetValue<bool>(kConfiguredInWorldSpaceKey);
		_joint.highAngularXLimit		= _info.GetValue<SoftJointLimit>(kHighAngularXLimitKey);
		_joint.linearLimit				= _info.GetValue<SoftJointLimit>(kLinearLimitKey);
		_joint.lowAngularXLimit			= _info.GetValue<SoftJointLimit>(kLowAngularXLimitKey);
		_joint.projectionAngle			= _info.GetValue<float>(kProjectionAngleKey);
		_joint.projectionDistance		= _info.GetValue<float>(kProjectionDistanceKey);
		_joint.projectionMode			= _info.GetValue<JointProjectionMode>(kProjectionModeKey);
		_joint.rotationDriveMode		= _info.GetValue<RotationDriveMode>(kRotationDriveModeKey);
		_joint.secondaryAxis			= _info.GetValue<Vector3>(kSecondaryAxisKey);
		_joint.slerpDrive				= _info.GetValue<JointDrive>(kSlerpDriveKey);
		_joint.swapBodies				= _info.GetValue<bool>(kSwapBodiesKey);
		_joint.targetAngularVelocity	= _info.GetValue<Vector3>(kTargetAngularVelocityKey);
		_joint.targetPosition			= _info.GetValue<Vector3>(kTargetPositionKey);
		_joint.targetRotation			= _info.GetValue<Quaternion>(kTargetRotationKey);
		_joint.targetVelocity			= _info.GetValue<Vector3>(kTargetVelocityKey);
		_joint.xDrive					= _info.GetValue<JointDrive>(kXDriveKey);
		_joint.xMotion					= _info.GetValue<ConfigurableJointMotion>(kXMotionKey);
		_joint.yDrive					= _info.GetValue<JointDrive>(kYDriveKey);
		_joint.yMotion					= _info.GetValue<ConfigurableJointMotion>(kYMotionKey);
		_joint.zDrive					= _info.GetValue<JointDrive>(kZDriveKey);
		_joint.zMotion					= _info.GetValue<ConfigurableJointMotion>(kZMotionKey);

#if UNITY_5
		_joint.angularXLimitSpring		= _info.GetValue<SoftJointLimitSpring>(kAngularXLimitSpringKey);
		_joint.angularYZLimitSpring		= _info.GetValue<SoftJointLimitSpring>(kAngularYZLimitSpringKey);
		_joint.linearLimitSpring		= _info.GetValue<SoftJointLimitSpring>(kLinearLimitSpringKey);
#endif

		return _joint;
	}

	#endregion
}