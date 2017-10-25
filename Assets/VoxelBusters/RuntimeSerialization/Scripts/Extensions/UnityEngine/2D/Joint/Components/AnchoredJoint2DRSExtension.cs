using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class AnchoredJoint2DRSExtension : Joint2DRSExtension 
{
	#region Constants
	
	private 	const	string		kAnchorKey				= "anchor";
	private 	const	string		kConnectedAnchorKey		= "connectedAnchor";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		AnchoredJoint2D		_joint	= _object as AnchoredJoint2D;
		
		if (_joint == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<Vector2>(kAnchorKey, 			_joint.anchor);
		_info.AddValue<Vector2>(kConnectedAnchorKey, 	_joint.connectedAnchor);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		AnchoredJoint2D 	_joint	= base.ReadSerializationData(_object, _info) as AnchoredJoint2D;
		
		if (_joint == null)
			return null;

		// Deserialize properties
		_joint.anchor				= _info.GetValue<Vector2>(kAnchorKey);	
		_joint.connectedAnchor		= _info.GetValue<Vector2>(kConnectedAnchorKey);

		return _joint;
	}
	
	#endregion
}