using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class JointRSExtension : ComponentRSExtension 
{
	#region Constants

	private 	const	string		kAnchorKey				= "anchor";
	private 	const	string		kAutoConfigureConnectedAnchorKey	= "autoConfigAnchor";
	private 	const	string		kAxisKey				= "axis";
	private 	const	string		kBreakForceKey			= "breakForce";
	private 	const	string		kBreakTorqueKey			= "breakTorque";
	private 	const	string		kConnectedAnchorKey		= "connectedAnchor";
	private 	const	string		kConnectedBodyKey		= "connectedBody";
	private 	const	string		kEnableCollisionKey		= "enableCollision";

#if UNITY_5
	private 	const	string		kEnablePreprocessingKey	= "enablePreprocessing";
#endif
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Joint		_joint			= _object as Joint;

		if (_joint == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<Vector3>(kAnchorKey,	 					_joint.anchor);
		_info.AddValue<bool>(kAutoConfigureConnectedAnchorKey,	_joint.autoConfigureConnectedAnchor);
		_info.AddValue<Vector3>(kAxisKey,	 					_joint.axis);
		_info.AddValue<float>(kBreakForceKey,	 				_joint.breakForce);
		_info.AddValue<float>(kBreakTorqueKey,	 				_joint.breakTorque);
		_info.AddValue<Vector3>(kConnectedAnchorKey,	 		_joint.connectedAnchor);
		_info.AddValue<GameObject>(kConnectedBodyKey,			_joint.connectedBody == null ? null : _joint.connectedBody.gameObject);
		_info.AddValue<bool>(kEnableCollisionKey,	 			_joint.enableCollision);

#if UNITY_5
		_info.AddValue<bool>(kEnablePreprocessingKey,	 		_joint.enablePreprocessing);
#endif
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Joint		_joint			= base.ReadSerializationData(_object, _info) as Joint;

		if (_joint == null)
			return null;
		
		// Deserialize properties
		_joint.anchor				= _info.GetValue<Vector3>(kAnchorKey);
		_joint.autoConfigureConnectedAnchor	= _info.GetValue<bool>(kAutoConfigureConnectedAnchorKey);
		_joint.axis					= _info.GetValue<Vector3>(kAxisKey);
		_joint.breakForce			= _info.GetValue<float>(kBreakForceKey);
		_joint.breakTorque			= _info.GetValue<float>(kBreakTorqueKey);
		_joint.connectedAnchor		= _info.GetValue<Vector3>(kConnectedAnchorKey);

		GameObject	_connectedGO	= _info.GetValue<GameObject>(kConnectedBodyKey);
		Rigidbody	_connectedBody	= null;

		if (_connectedGO != null)
			_connectedBody			= _connectedGO.GetComponent<Rigidbody>();

		_joint.connectedBody		= _connectedBody;
		_joint.enableCollision		= _info.GetValue<bool>(kEnableCollisionKey);

#if UNITY_5
		_joint.enablePreprocessing	= _info.GetValue<bool>(kEnablePreprocessingKey);
#endif
		return _joint;
	}

	#endregion
}