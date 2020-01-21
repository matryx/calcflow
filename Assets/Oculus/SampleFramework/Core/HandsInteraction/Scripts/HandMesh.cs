/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.

See SampleFramework license.txt for license terms.  Unless required by applicable law
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific
language governing permissions and limitations under the license.

************************************************************************************/

using System.Collections;
using UnityEngine;

namespace OculusSampleFramework
{
	public class HandMesh : MonoBehaviour
	{
		private static int _handAlphaId = Shader.PropertyToID("_HandAlpha");
		private const float MAX_ALPHA = 1.0F;
		private const float TRANSLUCENT_ALPHA = 0.75F;
		public Mesh Mesh { get; private set; }
		private SkinnedMeshRenderer _skinnedMeshRenderer;
		private MeshFilter _meshFilter;
		private Material _material;
		private Hand _hand;
		private bool _isInitialized = false;
		private float _currentAlphaMax = MAX_ALPHA;
		private HandMaterialModifier _handMaterialModifier;
		private bool _enableMeshVisual = true;

		public bool IsInitialized
		{
			get
			{
				return _isInitialized;
			}
			private set
			{
				_isInitialized = value;
			}
		}

		public SkinnedMeshRenderer HandSkinedMeshRenderer
		{
			get
			{
				return _skinnedMeshRenderer;
			}
			private set
			{
				_skinnedMeshRenderer = value;
			}
		}

		void Awake()
		{
			_skinnedMeshRenderer = GetComponent<SkinnedMeshRenderer>();
			if (!_skinnedMeshRenderer)
			{
				_skinnedMeshRenderer = gameObject.AddComponent<SkinnedMeshRenderer>();
			}
			_meshFilter = GetComponent<MeshFilter>();
			if (!_meshFilter)
			{
				_meshFilter = gameObject.AddComponent<MeshFilter>();
			}
			Mesh = new Mesh();
			_meshFilter.sharedMesh = Mesh;

			_hand = GetComponent<Hand>();
			_material = _skinnedMeshRenderer.material;
			_handMaterialModifier = new HandMaterialModifier(_material);
		}

		private void OnDestroy()
		{
			if (_meshFilter)
			{
				_meshFilter.sharedMesh = null;
			}
			if (Mesh)
			{
				Destroy(Mesh);
			}

			if (_material != null)
			{
				Destroy(_material);
			}
		}

		private void Start()
		{
			StartCoroutine(InitializeHandMesh());
		}

		private IEnumerator InitializeHandMesh()
		{
			bool success = false;
			while (!success)
			{
				var mesh = new OVRPlugin.Mesh();
				OVRPlugin.MeshType meshType = GetHandMeshTypeFromOVRHandType(_hand.HandType);
				if (OVRPlugin.GetMesh(meshType, out mesh))
				{
					success = InitializeMesh(ref mesh);
				}
				yield return null;
			}
		}

		private bool InitializeMesh(ref OVRPlugin.Mesh mesh)
		{
			// copy vertices
			var vertices = new Vector3[mesh.NumVertices];
			for (int i = 0; i < vertices.Length; ++i)
			{
				vertices[i] = mesh.VertexPositions[i].FromFlippedZVector3f();
			}
			Mesh.vertices = vertices;
			// copy uv
			var uv = new Vector2[mesh.NumVertices];
			for (int i = 0; i < uv.Length; ++i)
			{
				uv[i] = new Vector2(mesh.VertexUV0[i].x, -mesh.VertexUV0[i].y);
			}
			Mesh.uv = uv;
			// copy triangles
			var trianglesLength = mesh.NumIndices;
			var triangles = new int[trianglesLength];
			for (int i = 0; i < trianglesLength; ++i)
			{
				triangles[i] = mesh.Indices[trianglesLength - i - 1];
			}
			Mesh.triangles = triangles;
			// copy normals
			var normalsLength = mesh.NumVertices;
			var normals = new Vector3[normalsLength];
			for (int i = 0; i < normalsLength; ++i)
			{
				normals[i] = mesh.VertexNormals[i].FromFlippedZVector3f();
			}
			Mesh.normals = normals;

			Mesh.boneWeights = GetMeshBoneWeights(ref mesh);
			HandSkinedMeshRenderer.sharedMesh = Mesh;
			HandSkinedMeshRenderer.updateWhenOffscreen = true;
			_isInitialized = true;
			return true;
		}

		private static BoneWeight[] GetMeshBoneWeights(ref OVRPlugin.Mesh mesh)
		{
			BoneWeight[] ws = new BoneWeight[mesh.NumVertices];
			for (int i = 0; i < ws.Length; ++i)
			{
				var currentBlendWeight = mesh.BlendWeights[i];
				var currentBlendIndices = mesh.BlendIndices[i];
				ws[i].boneIndex0 = (int)currentBlendIndices.x;
				ws[i].weight0 = currentBlendWeight.x;
				ws[i].boneIndex1 = (int)currentBlendIndices.y;
				ws[i].weight1 = currentBlendWeight.y;
				ws[i].boneIndex2 = (int)currentBlendIndices.z;
				ws[i].weight2 = currentBlendWeight.z;
				ws[i].boneIndex3 = (int)currentBlendIndices.w;
				ws[i].weight3 = currentBlendWeight.w;
			}
			return ws;
		}

		public void UpdatePose()
		{
			bool showHandMesh = _enableMeshVisual ? _hand.IsTracked : _enableMeshVisual;
			if (HandSkinedMeshRenderer.enabled != showHandMesh)
			{
				HandSkinedMeshRenderer.enabled = showHandMesh;
			}
			// getting alpha value according to current scaled value and current allowed max alpha value.
			var scaledValue = Mathf.Clamp(_hand.ScaledAlpha, 0.0f, _currentAlphaMax);
			_material.SetFloat(_handAlphaId, scaledValue);
			// Update hand material according to system gesture state
			_handMaterialModifier.UpdateHandMaterial(_hand, _material);
		}

		public void EnableMeshRenderer(Hands.HandsVisualMode mode)
		{
			switch (mode)
			{
				case Hands.HandsVisualMode.Mesh:
					_enableMeshVisual = _hand.IsTracked;
					_currentAlphaMax = MAX_ALPHA;
					break;
				case Hands.HandsVisualMode.Skeleton:
					_enableMeshVisual = false;
					break;
				case Hands.HandsVisualMode.Both:
					_enableMeshVisual = _hand.IsTracked;
					_currentAlphaMax = TRANSLUCENT_ALPHA;
					break;
				default:
					break;
			}
		}

		public static OVRPlugin.MeshType GetHandMeshTypeFromOVRHandType(OVRPlugin.Hand hand)
		{
			return hand == OVRPlugin.Hand.HandLeft ?
			  OVRPlugin.MeshType.HandLeft :
			  hand == OVRPlugin.Hand.HandRight ?
			  OVRPlugin.MeshType.HandRight : OVRPlugin.MeshType.None;
		}

	}
}
