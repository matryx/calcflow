using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class NavMeshAgentRSExtension : BehaviourRSExtension 
{
	#region Constants
	
	private		const	string		kAccelerationKey			= "acceleration";
	private 	const	string		kAngularSpeedKey			= "angularSpeed";
	private 	const	string		kAutoBrakingKey				= "autoBraking";
	private 	const	string		kAutoRepathKey				= "autoRepath";
	private 	const	string		kAutoTraverseOffMeshLinkKey	= "autoTraverse";
	private 	const	string		kAvoidancePriorityKey		= "avoidancePriority";
	private 	const	string		kBaseOffsetKey				= "baseOffset";
	private 	const	string		kDestinationKey				= "destination";
	private 	const	string		kHeightKey					= "height";
	private 	const	string		kNextPositionKey			= "nextPosition";
	private 	const	string		kObstacleAvoidanceTypeKey	= "obstacleAvoidanceType";
	private 	const	string		kRadiusKey					= "radius";
	private 	const	string		kSpeedKey					= "speed";
	private 	const	string		kStoppingDistanceKey		= "stoppingDistance";
	private 	const	string		kUpdatePositionKey			= "updatePosition";
	private 	const	string		kUpdateRotationKey			= "updateRotation";
	private 	const	string		kVelocityKey				= "velocity";

#if UNITY_4_6 || UNITY_4_7
	private 	const	string		kWalkableMaskKey			= "walkableMask";
#elif UNITY_5
	private 	const	string		kAreaMaskKey				= "areaMask";
#endif
	
	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		UnityEngine.AI.NavMeshAgent 	_navMeshAgent		= _object as UnityEngine.AI.NavMeshAgent;

		if (_navMeshAgent == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<float>(kAccelerationKey,								_navMeshAgent.acceleration);
		_info.AddValue<float>(kAngularSpeedKey, 							_navMeshAgent.angularSpeed);
		_info.AddValue<bool>(kAutoBrakingKey, 								_navMeshAgent.autoBraking);
		_info.AddValue<bool>(kAutoRepathKey, 								_navMeshAgent.autoRepath);
		_info.AddValue<bool>(kAutoTraverseOffMeshLinkKey, 					_navMeshAgent.autoTraverseOffMeshLink);
		_info.AddValue<int>(kAvoidancePriorityKey, 							_navMeshAgent.avoidancePriority);
		_info.AddValue<float>(kBaseOffsetKey, 								_navMeshAgent.baseOffset);		
		_info.AddValue<Vector3>(kDestinationKey, 							_navMeshAgent.destination);
		_info.AddValue<float>(kHeightKey, 									_navMeshAgent.height);
		_info.AddValue<Vector3>(kNextPositionKey, 							_navMeshAgent.nextPosition);
		_info.AddValue<UnityEngine.AI.ObstacleAvoidanceType>(kObstacleAvoidanceTypeKey, 	_navMeshAgent.obstacleAvoidanceType);
		_info.AddValue<float>(kRadiusKey, 									_navMeshAgent.radius);
		_info.AddValue<float>(kSpeedKey, 									_navMeshAgent.speed);
		_info.AddValue<float>(kStoppingDistanceKey, 						_navMeshAgent.stoppingDistance);
		_info.AddValue<bool>(kUpdatePositionKey, 							_navMeshAgent.updatePosition);
		_info.AddValue<bool>(kUpdateRotationKey, 							_navMeshAgent.updateRotation);
		_info.AddValue<Vector3>(kVelocityKey, 								_navMeshAgent.velocity);

#if UNITY_4_6 || UNITY_4_7
		_info.AddValue<int>(kWalkableMaskKey, 								_navMeshAgent.walkableMask);
#elif UNITY_5
		_info.AddValue<int>(kAreaMaskKey, 									_navMeshAgent.areaMask);
#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		UnityEngine.AI.NavMeshAgent 	_navMeshAgent				=  base.ReadSerializationData(_object, _info) as UnityEngine.AI.NavMeshAgent;

		if (_navMeshAgent == null)
			return null;

		// Deserialize properties
		_navMeshAgent.acceleration					= _info.GetValue<float>(kAccelerationKey);
		_navMeshAgent.angularSpeed					= _info.GetValue<float>(kAngularSpeedKey);
		_navMeshAgent.autoBraking					= _info.GetValue<bool>(kAutoBrakingKey);
		_navMeshAgent.autoRepath					= _info.GetValue<bool>(kAutoRepathKey);
		_navMeshAgent.autoTraverseOffMeshLink		= _info.GetValue<bool>(kAutoTraverseOffMeshLinkKey);
		_navMeshAgent.avoidancePriority				= _info.GetValue<int>(kAvoidancePriorityKey);
		_navMeshAgent.baseOffset					= _info.GetValue<float>(kBaseOffsetKey);		
		_navMeshAgent.destination					= _info.GetValue<Vector3>(kDestinationKey);
		_navMeshAgent.height						= _info.GetValue<float>(kHeightKey);
		_navMeshAgent.nextPosition					= _info.GetValue<Vector3>(kNextPositionKey);
		_navMeshAgent.obstacleAvoidanceType			= _info.GetValue<UnityEngine.AI.ObstacleAvoidanceType>(kObstacleAvoidanceTypeKey);
		_navMeshAgent.radius						= _info.GetValue<float>(kRadiusKey);
		_navMeshAgent.speed							= _info.GetValue<float>(kSpeedKey);
		_navMeshAgent.stoppingDistance				= _info.GetValue<float>(kStoppingDistanceKey);
		_navMeshAgent.updatePosition				= _info.GetValue<bool>(kUpdatePositionKey);
		_navMeshAgent.updateRotation				= _info.GetValue<bool>(kUpdateRotationKey);
		_navMeshAgent.velocity						= _info.GetValue<Vector3>(kVelocityKey);
		
#if UNITY_4_6 || UNITY_4_7
		_navMeshAgent.walkableMask					= _info.GetValue<int>(kWalkableMaskKey);
#elif UNITY_5
		_navMeshAgent.areaMask						= _info.GetValue<int>(kAreaMaskKey);
#endif

		return _navMeshAgent;
	}
	
	#endregion
}