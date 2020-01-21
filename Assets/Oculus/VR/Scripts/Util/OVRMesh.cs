/************************************************************************************
Copyright : Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.

Licensed under the Oculus Utilities SDK License Version 1.31 (the "License"); you may not use
the Utilities SDK except in compliance with the License, which is provided at the time of installation
or download, or which otherwise accompanies this software in either electronic or hard copy form.

You may obtain a copy of the License at
https://developer.oculus.com/licenses/utilities-1.31

Unless required by applicable law or agreed to in writing, the Utilities SDK distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF
ANY KIND, either express or implied. See the License for the specific language governing
permissions and limitations under the License.
************************************************************************************/

using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[DefaultExecutionOrder(-80)]
public class OVRMesh : MonoBehaviour
{
	public interface IOVRMeshDataProvider
	{
		MeshType GetMeshType();
	}

	public enum MeshType
	{
		None = OVRPlugin.MeshType.None,
		HandLeft = OVRPlugin.MeshType.HandLeft,
		HandRight = OVRPlugin.MeshType.HandRight,
	}

	[SerializeField]
	private IOVRMeshDataProvider _dataProvider;
	[SerializeField]
	private MeshType _meshType = MeshType.None;
	private Mesh _mesh;

	public Mesh Mesh
	{
		get { return _mesh; }
	}

	private void Awake()
	{
		if (_mesh != null)
		{
			// simply act as a mesh reference if a custom mesh is specified
			return;
		}

		if (_dataProvider == null)
		{
			_dataProvider = GetComponent<IOVRMeshDataProvider>();
		}

		if (_dataProvider != null)
		{
			_meshType = _dataProvider.GetMeshType();
		}

		if (_meshType != MeshType.None)
		{
			Initialize(_meshType);
		}
	}

	private void Initialize(MeshType meshType)
	{
		_mesh = new Mesh();

		var ovrpMesh = new OVRPlugin.Mesh();
		if (OVRPlugin.GetMesh((OVRPlugin.MeshType)_meshType, out ovrpMesh))
		{
			var vertices = new Vector3[ovrpMesh.NumVertices];
			for (int i = 0; i < ovrpMesh.NumVertices; ++i)
			{
				vertices[i] = ovrpMesh.VertexPositions[i].FromFlippedZVector3f();
			}
			_mesh.vertices = vertices;

			var uv = new Vector2[ovrpMesh.NumVertices];
			for (int i = 0; i < ovrpMesh.NumVertices; ++i)
			{
				uv[i] = new Vector2(ovrpMesh.VertexUV0[i].x, -ovrpMesh.VertexUV0[i].y);
			}
			_mesh.uv = uv;

			var triangles = new int[ovrpMesh.NumIndices];
			for (int i = 0; i < ovrpMesh.NumIndices; ++i)
			{
				triangles[i] = ovrpMesh.Indices[ovrpMesh.NumIndices - i - 1];
			}
			_mesh.triangles = triangles;

			var normals = new Vector3[ovrpMesh.NumVertices];
			for (int i = 0; i < ovrpMesh.NumVertices; ++i)
			{
				normals[i] = ovrpMesh.VertexNormals[i].FromFlippedZVector3f();
			}
			_mesh.normals = normals;

			var boneWeights = new BoneWeight[ovrpMesh.NumVertices];
			for (int i = 0; i < ovrpMesh.NumVertices; ++i)
			{
				var currentBlendWeight = ovrpMesh.BlendWeights[i];
				var currentBlendIndices = ovrpMesh.BlendIndices[i];

				boneWeights[i].boneIndex0 = (int)currentBlendIndices.x;
				boneWeights[i].weight0 = currentBlendWeight.x;
				boneWeights[i].boneIndex1 = (int)currentBlendIndices.y;
				boneWeights[i].weight1 = currentBlendWeight.y;
				boneWeights[i].boneIndex2 = (int)currentBlendIndices.z;
				boneWeights[i].weight2 = currentBlendWeight.z;
				boneWeights[i].boneIndex3 = (int)currentBlendIndices.w;
				boneWeights[i].weight3 = currentBlendWeight.w;
			}
			_mesh.boneWeights = boneWeights;
		}
	}
}
