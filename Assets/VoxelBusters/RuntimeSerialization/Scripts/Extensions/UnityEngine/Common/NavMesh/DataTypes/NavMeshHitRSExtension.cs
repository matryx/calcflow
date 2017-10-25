using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class NavMeshHitRSExtension : IRuntimeSerializableExtension 
{
	#region Constants
	
	private		const	string		kDistanceKey			= "distance";
	private 	const	string		kHitKey					= "hit";
	private 	const	string		kMaskKey				= "mask";
	private 	const	string		kNormalKey				= "normal";
	private 	const	string		kPositionKey			= "position";

	#endregion
	
	#region Serialization Methods

	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		UnityEngine.AI.NavMeshHit 	_navMeshHit		= (UnityEngine.AI.NavMeshHit)_object;
		
		// Serialize properties
		_info.AddValue<float>(kDistanceKey, 	_navMeshHit.distance);
		_info.AddValue<bool>(kHitKey, 			_navMeshHit.hit);
		_info.AddValue<int>(kMaskKey, 			_navMeshHit.mask);
		_info.AddValue<Vector3>(kNormalKey, 	_navMeshHit.normal);
		_info.AddValue<Vector3>(kPositionKey, 	_navMeshHit.position);
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		UnityEngine.AI.NavMeshHit 	_navMeshHit		= (UnityEngine.AI.NavMeshHit)_object;

		// Deserialize properties
		_navMeshHit.distance		= _info.GetValue<float>(kDistanceKey);
		_navMeshHit.hit				= _info.GetValue<bool>(kHitKey);
		_navMeshHit.mask			= _info.GetValue<int>(kMaskKey);
		_navMeshHit.normal			= _info.GetValue<Vector3>(kNormalKey);
		_navMeshHit.position		= _info.GetValue<Vector3>(kPositionKey);
		
		return _navMeshHit;
	}

	#endregion
}