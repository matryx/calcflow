/************************************************************************************

Copyright (c) Facebook Technologies, LLC and its affiliates. All rights reserved.  

See SampleFramework license.txt for license terms.  Unless required by applicable law 
or agreed to in writing, the sample code is provided “AS IS” WITHOUT WARRANTIES OR 
CONDITIONS OF ANY KIND, either express or implied.  See the license for specific 
language governing permissions and limitations under the license.

************************************************************************************/

using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Gameobject that displays the raw skeleton 
 **/
namespace OculusSampleFramework
{
	public class SkeletonVisualizer : MonoBehaviour
	{
		private static int HAND_ALPHA_ID = Shader.PropertyToID("_HandAlpha");
		private const float BONE_WIDTH = 0.01f;
		private const float BONE_HEIGHT = 0.001f;
		private const float LINE_RENDERER_WIDTH = 0.005f;
		[SerializeField] private Material _skeletonMaterial;
		public bool EnableVisualization = false;
		private List<BoneBounds> _boneConnectorsVisualizers;
		private Hand _hand;
		private GameObject _skeleton;

		private bool _isInitialized = false;
		private HandMaterialModifier _handMaterialModifier;

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
		private class BoneBounds
		{
			public GameObject StartBone;
			public GameObject BoneVisual;
			public GameObject EndSphere;
			public LineRenderer Line;

			public BoneBounds(GameObject parent1, GameObject bone, GameObject parent2, LineRenderer line)
			{
				StartBone = parent1;
				BoneVisual = bone;
				EndSphere = parent2;
				Line = line;
			}
		}

		private void Awake()
		{
			_hand = GetComponent<Hand>();
			_boneConnectorsVisualizers = new List<BoneBounds>();
			_skeleton = new GameObject("SkeletonVisualizer");
			_skeleton.transform.SetParent(transform, false);
			_skeletonMaterial = Instantiate(_skeletonMaterial) as Material;
			StartCoroutine(BuildSkeletonVis());

			_handMaterialModifier = new HandMaterialModifier(_skeletonMaterial);
		}

		private void OnDestroy()
		{
			if (_skeletonMaterial)
			{
				Destroy(_skeletonMaterial);
			}
			if (_skeleton)
			{
				Destroy(_skeleton);
			}
		}

		private void LateUpdate()
		{
			if (!IsInitialized || !_hand.IsInitialized)
			{
				return;
			}
			if (_skeleton.activeSelf != EnableVisualization)
			{
				_skeleton.SetActive(EnableVisualization);
			}
			if (!EnableVisualization)
			{
				return;
			}
			float width = LINE_RENDERER_WIDTH * _hand.State.HandScale;
			//Update bone connectors
			for (int i = 0; i < _boneConnectorsVisualizers.Count; i++)
			{
				_boneConnectorsVisualizers[i].Line.SetPosition(0, _boneConnectorsVisualizers[i].StartBone.transform.position);
				_boneConnectorsVisualizers[i].Line.SetPosition(1, _boneConnectorsVisualizers[i].EndSphere.transform.position);
				_boneConnectorsVisualizers[i].Line.startWidth = width;
				_boneConnectorsVisualizers[i].Line.endWidth = width;
			}
			//set alpha
			var alpha = _hand.ScaledAlpha > 0.5f ? 1.0f : Mathf.Clamp(_hand.ScaledAlpha, 0.0f, 1.0f);
			_skeletonMaterial.SetFloat(HAND_ALPHA_ID, alpha);
			//Update material to identify trigger of system gesture.
			_handMaterialModifier.UpdateHandMaterial(_hand, _skeletonMaterial);
		}

		private int GetBoneParentIndex(Transform bone)
		{
			Transform parentT = bone.parent;
			for (int i = 0; i < _hand.Skeleton.Bones.Count; i++)
			{
				if (_hand.Skeleton.Bones[i] == parentT)
				{
					return i;
				}
			}
			return -1;
		}

		private IEnumerator BuildSkeletonVis()
		{
			while (!_hand.Skeleton.IsInitialized)
			{
				yield return null;
			}
			//build the bone visual.
			for (int i = 0; i < (int)OVRPlugin.BoneId.Max; ++i)
			{
				var bone = _hand.Skeleton.Bones[i];
				int parentIndex = GetBoneParentIndex(bone);
				if (parentIndex < 0)
				{
					continue;
				}
				else
				{
					Transform parent = _hand.Skeleton.Bones[parentIndex];
					BuildBone(bone.gameObject, parent.gameObject);
				}
			}
			IsInitialized = true;
		}

		//Build bone connector visual
		private void BuildBone(GameObject boneVis, GameObject parent)
		{
			var boneVisual = new GameObject();
			var line = CreateLineRenderer(boneVisual, true);
			boneVisual.transform.SetParent(_skeleton.transform, false);
			line.SetPosition(0, parent.transform.position);
			line.SetPosition(1, boneVis.transform.position);
			_boneConnectorsVisualizers.Add(new BoneBounds(parent, boneVisual, boneVis, line));
		}

		private LineRenderer CreateLineRenderer(GameObject addToGameObject, bool useWorldSpace)
		{
			var line = addToGameObject.AddComponent<LineRenderer>();
			line.sharedMaterial = _skeletonMaterial;
			line.startWidth = LINE_RENDERER_WIDTH * _hand.State.HandScale;
			line.endWidth = LINE_RENDERER_WIDTH * _hand.State.HandScale;
			line.useWorldSpace = useWorldSpace;
			line.positionCount = 2;
			return line;
		}
	}
}
