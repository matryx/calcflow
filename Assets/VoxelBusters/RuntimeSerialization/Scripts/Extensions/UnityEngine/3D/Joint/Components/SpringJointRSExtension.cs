using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class SpringJointRSExtension : JointRSExtension 
{
	#region Constants
	
	private 	const	string		kDamperKey				= "damper";
	private 	const	string		kMaxDistanceKey			= "maxDistance";
	private 	const	string		kMinDistanceKey			= "minDistance";
	private 	const	string		kSpringKey				= "spring";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SpringJoint	_joint		= _object as SpringJoint;

		if (_joint == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<float>(kDamperKey,	 	_joint.damper);
		_info.AddValue<float>(kMaxDistanceKey,	_joint.maxDistance);
		_info.AddValue<float>(kMinDistanceKey, 	_joint.minDistance);
		_info.AddValue<float>(kSpringKey,	 	_joint.spring);
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SpringJoint	_joint		= base.ReadSerializationData(_object, _info) as SpringJoint;
		
		if (_joint == null)
			return null;

		// Deserialize properties
		_joint.damper			= _info.GetValue<float>(kDamperKey);
		_joint.maxDistance		= _info.GetValue<float>(kMaxDistanceKey);
		_joint.minDistance		= _info.GetValue<float>(kMinDistanceKey);
		_joint.spring			= _info.GetValue<float>(kSpringKey);

		return _joint;
	}

	#endregion
}