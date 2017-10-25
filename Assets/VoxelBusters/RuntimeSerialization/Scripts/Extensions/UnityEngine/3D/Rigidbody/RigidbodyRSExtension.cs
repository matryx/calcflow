using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class RigidbodyRSExtension : ComponentRSExtension 
{
	#region Constants
	
	private 	const	string		kAngularDragKey						= "angularDrag";
	private 	const	string		kAngularVelocityKey					= "angularVelocity";
	private 	const	string		kCenterOfMassKey					= "centerOfMass";
	private 	const	string		kCollisionDetectionModeKey			= "collisionDetectionMode";
	private 	const	string		kConstraintsKey						= "constraints";
	private 	const	string		kDetectCollisionsKey				= "detectCollisions";
	private 	const	string		kDragKey							= "drag";
	private 	const	string		kFreezeRotationKey					= "freezeRotation";
	private 	const	string		kInertiaTensorKey					= "inertiaTensor";
	private 	const	string		kInertiaTensorRotationKey			= "inertiaTensorRotation";
	private 	const	string		kInterpolationKey					= "interpolation";
	private 	const	string		kIsKinematicKey						= "isKinematic";
	private 	const	string		kMassKey							= "mass";
	private 	const	string		kMaxAngularVelocityKey				= "maxAngularVelocity";
	private 	const	string		kPositionKey						= "position";
	private 	const	string		kRotationKey						= "rotation";
	private 	const	string		kSolverIterationCountKey			= "solverIterationCount";
	private 	const	string		kUseConeFrictionKey					= "useConeFriction";
	private 	const	string		kUseGravityKey						= "useGravity";
	private 	const	string		kVelocityKey						= "velocity";

#if UNITY_4_6 || UNITY_4_7
	private 	const	string		kSleepAngularVelocityKey			= "sleepAngularVelocity";
	private 	const	string		kSleepVelocityKey					= "sleepVelocity";
#elif UNITY_5
	private 	const	string		kMaxDepenetrationVelocity 			= "maxDepenetrationVelocity";
	private 	const	string		kSleepThreshold						= "sleepThreshold";
#endif

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Rigidbody _rigidBody	= _object as Rigidbody;

		if (_rigidBody == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<float>(kAngularDragKey, 								_rigidBody.angularDrag);					
		_info.AddValue<Vector3>(kAngularVelocityKey, 						_rigidBody.angularVelocity);				
		_info.AddValue<Vector3>(kCenterOfMassKey, 							_rigidBody.centerOfMass);				
		_info.AddValue<CollisionDetectionMode>(kCollisionDetectionModeKey, 	_rigidBody.collisionDetectionMode);
		_info.AddValue<RigidbodyConstraints>(kConstraintsKey, 				_rigidBody.constraints);					
		_info.AddValue<bool>(kDetectCollisionsKey, 							_rigidBody.detectCollisions);			
		_info.AddValue<float>(kDragKey, 									_rigidBody.drag);						
		_info.AddValue<bool>(kFreezeRotationKey, 							_rigidBody.freezeRotation);				
		_info.AddValue<Vector3>(kInertiaTensorKey, 							_rigidBody.inertiaTensor);				
		_info.AddValue<Quaternion>(kInertiaTensorRotationKey, 				_rigidBody.inertiaTensorRotation);		
		_info.AddValue<RigidbodyInterpolation>(kInterpolationKey, 			_rigidBody.interpolation);				
		_info.AddValue<bool>(kIsKinematicKey, 								_rigidBody.isKinematic);					
		_info.AddValue<float>(kMassKey, 									_rigidBody.mass);						
		_info.AddValue<float>(kMaxAngularVelocityKey, 						_rigidBody.maxAngularVelocity);			
		_info.AddValue<Vector3>(kPositionKey, 								_rigidBody.position);					
		_info.AddValue<Quaternion>(kRotationKey, 							_rigidBody.rotation);					
		_info.AddValue<int>(kSolverIterationCountKey, 						_rigidBody.solverIterations);		
		_info.AddValue<bool>(kUseGravityKey, 								_rigidBody.useGravity);					
		_info.AddValue<Vector3>(kVelocityKey, 								_rigidBody.velocity);					

#if UNITY_4_6 || UNITY_4_7
		_info.AddValue<bool>(kUseConeFrictionKey, 							_rigidBody.useConeFriction);				
		_info.AddValue<float>(kSleepAngularVelocityKey, 					_rigidBody.sleepAngularVelocity);		
		_info.AddValue<float>(kSleepVelocityKey, 							_rigidBody.sleepVelocity);		
#elif UNITY_5
		_info.AddValue<float>(kMaxDepenetrationVelocity, 					_rigidBody.maxDepenetrationVelocity);					
		_info.AddValue<float>(kSleepThreshold, 								_rigidBody.sleepThreshold);		
#endif
	}

	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		Rigidbody _rigidBody				= base.ReadSerializationData(_object, _info) as Rigidbody;
		
		if (_rigidBody == null)
			return null;
		
		// Deserialize properties
		_rigidBody.angularDrag				= _info.GetValue<float>(kAngularDragKey);					
		_rigidBody.centerOfMass				= _info.GetValue<Vector3>(kCenterOfMassKey);				
		_rigidBody.collisionDetectionMode	= _info.GetValue<CollisionDetectionMode>(kCollisionDetectionModeKey);
		_rigidBody.constraints				= _info.GetValue<RigidbodyConstraints>(kConstraintsKey);					
		_rigidBody.detectCollisions			= _info.GetValue<bool>(kDetectCollisionsKey);			
		_rigidBody.drag						= _info.GetValue<float>(kDragKey);						
		_rigidBody.freezeRotation			= _info.GetValue<bool>(kFreezeRotationKey);				
		_rigidBody.inertiaTensor			= _info.GetValue<Vector3>(kInertiaTensorKey);				
		_rigidBody.inertiaTensorRotation	= _info.GetValue<Quaternion>(kInertiaTensorRotationKey);		
		_rigidBody.interpolation			= _info.GetValue<RigidbodyInterpolation>(kInterpolationKey);				
		_rigidBody.isKinematic				= _info.GetValue<bool>(kIsKinematicKey);					
		_rigidBody.mass						= _info.GetValue<float>(kMassKey);						
		_rigidBody.maxAngularVelocity		= _info.GetValue<float>(kMaxAngularVelocityKey);			
		_rigidBody.position					= _info.GetValue<Vector3>(kPositionKey);					
		_rigidBody.rotation					= _info.GetValue<Quaternion>(kRotationKey);					
		_rigidBody.solverIterations		= _info.GetValue<int>(kSolverIterationCountKey);		
		_rigidBody.useGravity				= _info.GetValue<bool>(kUseGravityKey);					

		// Actor must be (non-kinematic) dynamic!
		if (!_rigidBody.isKinematic)
		{
			_rigidBody.angularVelocity		= _info.GetValue<Vector3>(kAngularVelocityKey);				
			_rigidBody.velocity				= _info.GetValue<Vector3>(kVelocityKey);					
		}
		
#if UNITY_4_6 || UNITY_4_7
		_rigidBody.useConeFriction			= _info.GetValue<bool>(kUseConeFrictionKey);				
		_rigidBody.sleepAngularVelocity		= _info.GetValue<float>(kSleepAngularVelocityKey);		
		_rigidBody.sleepVelocity			= _info.GetValue<float>(kSleepVelocityKey);	
#elif UNITY_5
		_rigidBody.maxDepenetrationVelocity	= _info.GetValue<float>(kMaxDepenetrationVelocity);					
		_rigidBody.sleepThreshold			= _info.GetValue<float>(kSleepThreshold);		
#endif

		return _rigidBody;
	}

	#endregion
}