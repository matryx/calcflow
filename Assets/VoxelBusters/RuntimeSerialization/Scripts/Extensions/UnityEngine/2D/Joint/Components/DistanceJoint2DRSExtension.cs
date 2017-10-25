using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class DistanceJoint2DRSExtension : AnchoredJoint2DRSExtension 
{
	#region Constants
	
	private 	const	string		kDistanceKey			= "distance";
	private 	const	string		kMaxDistanceOnlyKey		= "maxDistance";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		DistanceJoint2D		_joint	= _object as DistanceJoint2D;
		
		if (_joint == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<float>(kDistanceKey, 			_joint.distance);
		_info.AddValue<bool>(kMaxDistanceOnlyKey, 		_joint.maxDistanceOnly);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		DistanceJoint2D		_joint	= base.ReadSerializationData(_object, _info) as DistanceJoint2D;
		
		if (_joint == null)
			return null;
		
		// Deserialize properties
		_joint.distance				= _info.GetValue<float>(kDistanceKey);	
		_joint.maxDistanceOnly		= _info.GetValue<bool>(kMaxDistanceOnlyKey);
		
		return _joint;
	}
	
	#endregion
}