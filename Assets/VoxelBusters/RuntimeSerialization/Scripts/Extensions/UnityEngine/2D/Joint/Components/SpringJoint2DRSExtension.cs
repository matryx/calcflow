using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class SpringJoint2DRSExtension : AnchoredJoint2DRSExtension 
{
	#region Constants
	
	private 	const	string		kDampingRatioKey	= "dampingRatio";
	private 	const	string		kDistanceKey		= "distance";
	private 	const	string		kFrequencyKey		= "frequency";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SpringJoint2D	_joint		= _object as SpringJoint2D;
		
		if (_joint == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serializing properties
		_info.AddValue<float>(kDampingRatioKey, 	_joint.dampingRatio);
		_info.AddValue<float>(kDistanceKey, 		_joint.distance);
		_info.AddValue<float>(kFrequencyKey, 		_joint.frequency);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		SpringJoint2D 	_joint		= base.ReadSerializationData(_object, _info) as SpringJoint2D;
		
		if (_joint == null)
			return null;
		
		// Deserialize properties
		_joint.dampingRatio			= _info.GetValue<float>(kDampingRatioKey);	
		_joint.distance				= _info.GetValue<float>(kDistanceKey);
		_joint.frequency			= _info.GetValue<float>(kFrequencyKey);
		
		return _joint;
	}
	
	#endregion
}