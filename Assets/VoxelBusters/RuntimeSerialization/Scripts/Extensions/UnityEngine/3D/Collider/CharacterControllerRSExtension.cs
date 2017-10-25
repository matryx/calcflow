using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class CharacterControllerRSExtension : ColliderRSExtension 
{
	#region Constants
	
	private 	const	string		kCenterKey				= "center";
	private 	const	string		kDetectCollisionsKey	= "detectCollisions";
	private 	const	string		kHeightKey				= "height";
	private 	const	string		kRadiusKey				= "radius";
	private 	const	string		kSlopeLimitKey			= "slopeLimit";
	private 	const	string		kStepOffsetKey			= "stepOffset";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CharacterController 	_characterController	= _object as CharacterController;
		
		if (_characterController == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<Vector3>(kCenterKey, 		_characterController.center);
		_info.AddValue<bool>(kDetectCollisionsKey, 	_characterController.detectCollisions);
		_info.AddValue<float>(kHeightKey, 			_characterController.height);
		_info.AddValue<float>(kRadiusKey, 			_characterController.radius);
		_info.AddValue<float>(kSlopeLimitKey, 		_characterController.slopeLimit);
		_info.AddValue<float>(kStepOffsetKey, 		_characterController.stepOffset);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		CharacterController 	_characterController	= base.ReadSerializationData(_object, _info) as CharacterController;
		
		if (_characterController == null)
			return null;
		
		// Deserialize properties
		_characterController.center						= _info.GetValue<Vector3>(kCenterKey);
		_characterController.detectCollisions			= _info.GetValue<bool>(kDetectCollisionsKey);
		_characterController.height						= _info.GetValue<float>(kHeightKey);
		_characterController.radius						= _info.GetValue<float>(kRadiusKey);
		_characterController.slopeLimit					= _info.GetValue<float>(kSlopeLimitKey);
		_characterController.stepOffset					= _info.GetValue<float>(kStepOffsetKey);
		
		return _characterController;
	}
	
	#endregion
}