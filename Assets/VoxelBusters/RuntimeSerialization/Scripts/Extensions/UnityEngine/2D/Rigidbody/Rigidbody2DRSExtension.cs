using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class Rigidbody2DRSExtension : ComponentRSExtension 
{
	#region Constants
	
	private 	const	string		kAngularDragKey						= "angularDrag";
	private 	const	string		kAngularVelocityKey					= "angularVelocity";
	private 	const	string		kCenterOfMassKey					= "centerOfMass";
	private 	const	string		kCollisionDetectionModeKey			= "collisionDetectionMode";
	private 	const	string		kDragKey							= "drag";
	private 	const	string		kGravityScale						= "gravityScale";
	private 	const	string		kInertiaKey							= "inertia";
	private 	const	string		kInterpolationKey					= "interpolation";
	private 	const	string		kIsKinematicKey						= "isKinematic";
	private 	const	string		kMassKey							= "mass";
	private 	const	string		kMaxAngularVelocityKey				= "maxAngularVelocity";
	private 	const	string		kPositionKey						= "position";
	private 	const	string		kRotationKey						= "rotation";
	private 	const	string		kSimulatedKey						= "simulated";
	private 	const	string		kSleepModeKey						= "sleepMode";
	private 	const	string		kVelocityKey						= "velocity";
	
#if UNITY_4_6 || UNITY_4_7 || UNITY_5_0
	private 	const	string		kFixedAngleKey						= "fixedAngle";
#elif UNITY_5
	private 	const	string		kConstraintsKey						= "constraints";
	private 	const	string		kFreezeRotationKey					= "freezeRotation";
#endif

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Rigidbody2D _rigidBody	= _object as Rigidbody2D;

		if (_rigidBody == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);
		
		// Serialize properties
		_info.AddValue<float>(kAngularDragKey, 									_rigidBody.angularDrag);					
		_info.AddValue<float>(kAngularVelocityKey, 								_rigidBody.angularVelocity);				
		_info.AddValue<Vector2>(kCenterOfMassKey, 								_rigidBody.centerOfMass);				
		_info.AddValue<CollisionDetectionMode2D>(kCollisionDetectionModeKey, 	_rigidBody.collisionDetectionMode);
		_info.AddValue<float>(kDragKey, 										_rigidBody.drag);	
		_info.AddValue<float>(kGravityScale, 									_rigidBody.gravityScale);	
		_info.AddValue<float>(kInertiaKey, 										_rigidBody.inertia);		
		_info.AddValue<RigidbodyInterpolation2D>(kInterpolationKey, 			_rigidBody.interpolation);		
		_info.AddValue<bool>(kIsKinematicKey, 									_rigidBody.isKinematic);					
		_info.AddValue<float>(kMassKey, 										_rigidBody.mass);	
		_info.AddValue<Vector2>(kPositionKey, 									_rigidBody.position);					
		_info.AddValue<float>(kRotationKey, 									_rigidBody.rotation);		
		_info.AddValue<bool>(kSimulatedKey, 									_rigidBody.simulated);		
		_info.AddValue<RigidbodySleepMode2D>(kSleepModeKey, 					_rigidBody.sleepMode);				
		_info.AddValue<Vector2>(kVelocityKey, 									_rigidBody.velocity);						
		
#if UNITY_4_6 || UNITY_4_7 || UNITY_5_0
		_info.AddValue<bool>(kFixedAngleKey, 									_rigidBody.fixedAngle);					
#elif UNITY_5
		_info.AddValue<RigidbodyConstraints2D>(kConstraintsKey, 				_rigidBody.constraints);					
		_info.AddValue<bool>(kFreezeRotationKey, 								_rigidBody.freezeRotation);		
#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		Rigidbody2D _rigidBody				= base.ReadSerializationData(_object, _info) as Rigidbody2D;
		
		if (_rigidBody == null)
			return null;
		
		// Deserialize properties
		_rigidBody.angularDrag 				= _info.GetValue<float>(kAngularDragKey);					
		_rigidBody.centerOfMass				= _info.GetValue<Vector2>(kCenterOfMassKey);				
		_rigidBody.collisionDetectionMode	= _info.GetValue<CollisionDetectionMode2D>(kCollisionDetectionModeKey);
		_rigidBody.drag						= _info.GetValue<float>(kDragKey);	
		_rigidBody.gravityScale				= _info.GetValue<float>(kGravityScale);	
		_rigidBody.inertia					= _info.GetValue<float>(kInertiaKey);		
		_rigidBody.interpolation			= _info.GetValue<RigidbodyInterpolation2D>(kInterpolationKey);		
		_rigidBody.isKinematic				= _info.GetValue<bool>(kIsKinematicKey);					
		_rigidBody.mass						= _info.GetValue<float>(kMassKey);	
		_rigidBody.position					= _info.GetValue<Vector2>(kPositionKey);					
		_rigidBody.rotation					= _info.GetValue<float>(kRotationKey);		
		_rigidBody.simulated 				= _info.GetValue<bool>(kSimulatedKey);		
		_rigidBody.sleepMode				= _info.GetValue<RigidbodySleepMode2D>(kSleepModeKey);		
		
		// Actor must be (non-kinematic) dynamic!
		if (!_rigidBody.isKinematic)
		{
			_rigidBody.angularVelocity		= _info.GetValue<float>(kAngularVelocityKey);				
			_rigidBody.velocity				= _info.GetValue<Vector2>(kVelocityKey);					
		}

#if UNITY_4_6 || UNITY_4_7 || UNITY_5_0
		_rigidBody.fixedAngle				= _info.GetValue<bool>(kFixedAngleKey);					
#elif UNITY_5
		_rigidBody.constraints				= _info.GetValue<RigidbodyConstraints2D>(kConstraintsKey);					
		_rigidBody.freezeRotation			= _info.GetValue<bool>(kFreezeRotationKey);		
#endif
		
		return _rigidBody;
	}
	
	#endregion
}