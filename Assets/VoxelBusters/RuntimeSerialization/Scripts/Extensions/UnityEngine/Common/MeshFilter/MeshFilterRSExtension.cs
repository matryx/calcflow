using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class MeshFilterRSExtension : ComponentRSExtension 
{
	#region Constants
	
	private		const	string		kSharedMeshKey			= "sharedMesh";
	private 	const	string		kMeshKey				= "mesh";

	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		MeshFilter 	_meshFilter		= _object as MeshFilter;

		if (_meshFilter == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serializing properties
		Mesh 		_sharedMesh		= _meshFilter.sharedMesh;

		if (_sharedMesh != null)
			_info.AddValue<Mesh>(kSharedMeshKey, 	_sharedMesh);
		else
			_info.AddValue<Mesh>(kMeshKey, 			_meshFilter.mesh);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		MeshFilter 	_meshFilter		= base.ReadSerializationData(_object, _info) as MeshFilter;

		if (_meshFilter == null)
			return null;
		
		// Deserialize properties
		Mesh 		_sharedMesh		= _info.GetValue<Mesh>(kSharedMeshKey);

		if (_sharedMesh != null)
			_meshFilter.sharedMesh	= _sharedMesh;
		else
			_meshFilter.mesh		= _info.GetValue<Mesh>(kMeshKey);

		return _meshFilter;
	}

	#endregion
}