using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class Joint2DRSExtension : BehaviourRSExtension 
{
	#region Constants
	
	private 	const	string		kConnectedBodyKey		= "connectedBody";

#if UNITY_4_6 || UNITY_4_7 || UNITY_5_0
	private 	const	string		kCollideConnectedKey	= "collideConnected";
#elif UNITY_5
	private 	const	string		kEnableCollisionKey		= "enableCollision";
#endif

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Joint2D		_joint			= _object as Joint2D;
		
		if (_joint == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<GameObject>(kConnectedBodyKey,	 _joint.connectedBody == null ? null : _joint.connectedBody.gameObject);

#if UNITY_4_6 || UNITY_4_7 || UNITY_5_0
		_info.AddValue<bool>(kCollideConnectedKey,		_joint.collideConnected);
#elif UNITY_5
		_info.AddValue<bool>(kEnableCollisionKey,		_joint.enableCollision);
#endif
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Joint2D 		_joint		= base.ReadSerializationData(_object, _info) as Joint2D;
		
		if (_joint == null)
			return null;
		
		// Deserialize properties
		GameObject	_connectedGO	= _info.GetValue<GameObject>(kConnectedBodyKey);
		Rigidbody2D	_connectedBody	= null;
		
		if (_connectedGO != null)
			_connectedBody			= _connectedGO.GetComponent<Rigidbody2D>();
		
		_joint.connectedBody		= _connectedBody;

#if UNITY_4_6 || UNITY_4_7 || UNITY_5_0
		_joint.collideConnected		= _info.GetValue<bool>(kCollideConnectedKey);
#elif UNITY_5
		_joint.enableCollision		= _info.GetValue<bool>(kEnableCollisionKey);
#endif

		return _joint;
	}

	#endregion
}