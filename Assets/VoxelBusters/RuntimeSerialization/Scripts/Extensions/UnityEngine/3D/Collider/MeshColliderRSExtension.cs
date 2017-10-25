using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class MeshColliderRSExtension : ColliderRSExtension 
{
	#region Constants

	private 	const	string		kConvexKey					= "convex";
	private 	const	string		kSharedMeshKey				= "sharedMesh";
	private 	const	string		kSmoothSphereCollisionsKey	= "smoothSphereCollisions";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		MeshCollider 	_collider			= _object as MeshCollider;
		
		if (_collider == null)
			return;
		
		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serialize properties
		_info.AddValue<bool>(kConvexKey, 					_collider.convex);
		_info.AddValue<Mesh>(kSharedMeshKey, 				_collider.sharedMesh);
		#if UNITY_4_6 || UNITY_4_7
		_info.AddValue<bool>(kSmoothSphereCollisionsKey, 	_collider.smoothSphereCollisions);
		#endif
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		// Deserialize base properties
		MeshCollider 	_collider			= base.ReadSerializationData(_object, _info) as MeshCollider;
		
		if (_collider == null)
			return null;

		// Deserialize properties
		_collider.convex					= _info.GetValue<bool>(kConvexKey);
		_collider.sharedMesh				= _info.GetValue<Mesh>(kSharedMeshKey);
		#if UNITY_4_6 || UNITY_4_7
		_collider.smoothSphereCollisions	= _info.GetValue<bool>(kSmoothSphereCollisionsKey);
		#endif

		return _collider;
	}
	
	#endregion
}