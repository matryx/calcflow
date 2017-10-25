using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class NavMeshObstacleRSExtension : BehaviourRSExtension 
{
	#region Constants
	
	private		const	string		kCarvingKey						= "carving";
	private 	const	string		kCarvingMoveThresholdKey		= "carvingMoveThreshold";
	private 	const	string		kHeightKey						= "height";
	private 	const	string		kRadiusKey						= "radius";
	private 	const	string		kVelocityKey					= "velocity";

#if UNITY_5
	private 	const	string		kCarveOnlyStationaryKey			= "carveOnlyStationary";
	private 	const	string		kCarvingTimeToStationaryKey		= "carvingTimeToStationary";
	private 	const	string		kCenterKey						= "center";
	private 	const	string		kShapeKey						= "shape";
	private 	const	string		kSizeKey						= "size";
#endif
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		UnityEngine.AI.NavMeshObstacle 	_navMeshObstacle		= _object as UnityEngine.AI.NavMeshObstacle;

		if (_navMeshObstacle == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<bool>(kCarvingKey, 						_navMeshObstacle.carving);
		_info.AddValue<float>(kCarvingMoveThresholdKey, 		_navMeshObstacle.carvingMoveThreshold);
		_info.AddValue<float>(kHeightKey, 						_navMeshObstacle.height);
		_info.AddValue<float>(kRadiusKey, 						_navMeshObstacle.radius);
		_info.AddValue<Vector3>(kVelocityKey, 					_navMeshObstacle.velocity);

#if UNITY_5
		_info.AddValue<bool>(kCarveOnlyStationaryKey, 			_navMeshObstacle.carveOnlyStationary);
		_info.AddValue<float>(kCarvingTimeToStationaryKey, 		_navMeshObstacle.carvingTimeToStationary);
		_info.AddValue<Vector3>(kCenterKey, 					_navMeshObstacle.center);
		_info.AddValue<UnityEngine.AI.NavMeshObstacleShape>(kShapeKey,			_navMeshObstacle.shape);
		_info.AddValue<Vector3>(kSizeKey, 						_navMeshObstacle.size);
#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		UnityEngine.AI.NavMeshObstacle 	_navMeshObstacle		= base.ReadSerializationData(_object, _info) as UnityEngine.AI.NavMeshObstacle;

		if (_navMeshObstacle == null)
			return null;

		// Deserialize properties
		_navMeshObstacle.carving					= _info.GetValue<bool>(kCarvingKey);
		_navMeshObstacle.carvingMoveThreshold		= _info.GetValue<float>(kCarvingMoveThresholdKey);
		_navMeshObstacle.height						= _info.GetValue<float>(kHeightKey);
		_navMeshObstacle.radius						= _info.GetValue<float>(kRadiusKey);
		_navMeshObstacle.velocity					= _info.GetValue<Vector3>(kVelocityKey);

#if UNITY_5
		_navMeshObstacle.carveOnlyStationary		= _info.GetValue<bool>(kCarveOnlyStationaryKey);
		_navMeshObstacle.carvingTimeToStationary	= _info.GetValue<float>(kCarvingTimeToStationaryKey);
		_navMeshObstacle.center						= _info.GetValue<Vector3>(kCenterKey);
		_navMeshObstacle.shape						= _info.GetValue<UnityEngine.AI.NavMeshObstacleShape>(kShapeKey);
		_navMeshObstacle.size						= _info.GetValue<Vector3>(kSizeKey);
#endif
		
		return _navMeshObstacle;
	}
	
	#endregion
}