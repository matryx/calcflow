using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class MeshRSExtension : ObjectRSExtension 
{
	#region Constants

	private		const	string		kBindposesKey			= "bindposes";
	private		const	string		kBoneWeightsKey			= "boneWeights";
	private 	const	string		kColorsKey				= "colors";
	private 	const	string		kNormalsKey				= "normals";
	private		const	string		kSubMeshCountKey		= "sm-count";
	private		const	string		kSubMeshTopologyListKey	= "sm-topologies";
	private		const	string		kSubMeshIndicesListKey	= "sm-indices";
	private 	const	string		kTangentsKey			= "tangents";
	private 	const	string		kUVKey					= "uv";
	private 	const	string		kUV2Key					= "uv2";
	private		const	string		kVerticesKey			= "vertices";

	#endregion

	#region Instance Method
	
	public override object CreateInstance (RuntimeSerializationInfo _info)
	{
		return new Mesh();
	}
	
	#endregion

	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Mesh 		_mesh				= _object as Mesh;

		if (_mesh == null)
			return;

		// Serialize base properties
		base.WriteSerializationData(_object, _info);

		// Serializing properties
		_info.AddValue<Matrix4x4[]>(kBindposesKey, 				_mesh.bindposes);
		_info.AddValue<BoneWeight[]>(kBoneWeightsKey, 			_mesh.boneWeights);
		_info.AddValue<Color[]>(kColorsKey, 					_mesh.colors);
		_info.AddValue<Vector3[]>(kNormalsKey, 					_mesh.normals);
		_info.AddValue<Vector4[]>(kTangentsKey, 				_mesh.tangents);
		_info.AddValue<Vector2[]>(kUVKey, 						_mesh.uv);
		_info.AddValue<Vector2[]>(kUV2Key, 						_mesh.uv2);
		_info.AddValue<Vector3[]>(kVerticesKey, 				_mesh.vertices);

		// Serialize sub-mesh properties
		int				_subMeshCount			= _mesh.subMeshCount;
		MeshTopology[]	_subMeshTopologyList	= new MeshTopology[_subMeshCount];
		int[][]			_subMeshIndicesList		= new int[_subMeshCount][];
		
		for (int _iter = 0; _iter < _subMeshCount; _iter++)
		{
			_subMeshTopologyList[_iter]			= _mesh.GetTopology(_iter);
			_subMeshIndicesList[_iter]			= _mesh.GetIndices(_iter);
		}
		
		_info.AddValue<int>(kSubMeshCountKey, 					_subMeshCount);
		_info.AddValue<int[][]>(kSubMeshIndicesListKey, 		_subMeshIndicesList);
		_info.AddValue<MeshTopology[]>(kSubMeshTopologyListKey, _subMeshTopologyList);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		Mesh		_mesh			= base.ReadSerializationData(_object, _info) as Mesh;

		if (_mesh == null)
			return null;

		// Clear existing data
		_mesh.Clear();

		// Deserialize properties
		_mesh.vertices				= _info.GetValue<Vector3[]>(kVerticesKey);
		_mesh.colors				= _info.GetValue<Color[]>(kColorsKey);
		_mesh.normals				= _info.GetValue<Vector3[]>(kNormalsKey);
		_mesh.uv					= _info.GetValue<Vector2[]>(kUVKey);
		_mesh.uv2					= _info.GetValue<Vector2[]>(kUV2Key);
		_mesh.tangents				= _info.GetValue<Vector4[]>(kTangentsKey);
		_mesh.bindposes				= _info.GetValue<Matrix4x4[]>(kBindposesKey);
		_mesh.boneWeights			= _info.GetValue<BoneWeight[]>(kBoneWeightsKey);

		// Deserialize sub-mesh properties
		int				_subMeshCount			= _info.GetValue<int>(kSubMeshCountKey);
		MeshTopology[]	_subMeshTopologyList	= _info.GetValue<MeshTopology[]>(kSubMeshTopologyListKey);
		int[][]			_subMeshIndicesList		= _info.GetValue<int[][]>(kSubMeshIndicesListKey);

		// Set submesh properties
		_mesh.subMeshCount						= _subMeshCount;

		for (int _iter = 0; _iter < _subMeshCount; _iter++)
			_mesh.SetIndices(_subMeshIndicesList[_iter], _subMeshTopologyList[_iter], _iter);

		return _mesh;
	}

	#endregion
}