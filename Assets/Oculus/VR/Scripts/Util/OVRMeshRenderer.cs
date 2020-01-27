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

public class OVRMeshRenderer : MonoBehaviour
{
	public interface IOVRMeshRendererDataProvider
	{
		MeshRendererData GetMeshRendererData();
	}

	public struct MeshRendererData
	{
		public bool IsDataValid { get; set; }
		public bool IsDataHighConfidence { get; set; }
	}

	[SerializeField]
	private IOVRMeshRendererDataProvider _dataProvider;
	[SerializeField]
	private OVRMesh _ovrMesh;
	[SerializeField]
	private OVRSkeleton _ovrSkeleton;

	private SkinnedMeshRenderer _skinnedMeshRenderer;
	private bool _isInitialized;

	private void Awake()
	{
		if (_dataProvider == null)
		{
			_dataProvider = GetComponent<IOVRMeshRendererDataProvider>();
		}

		if (_ovrMesh == null)
		{
			_ovrMesh = GetComponent<OVRMesh>();
		}

		if (_ovrSkeleton == null)
		{
			_ovrSkeleton = GetComponent<OVRSkeleton>();
		}
	}

	private void Start()
	{
		if (_ovrMesh == null)
		{
			this.enabled = false;
			return;
		}

		Initialize();
	}

	private void Initialize()
	{
		_skinnedMeshRenderer = GetComponent<SkinnedMeshRenderer>();
		if (!_skinnedMeshRenderer)
		{
			_skinnedMeshRenderer = gameObject.AddComponent<SkinnedMeshRenderer>();
		}
		_skinnedMeshRenderer.sharedMesh = _ovrMesh.Mesh;

		if (_ovrSkeleton != null)
		{
			int numSkinnableBones = _ovrSkeleton.GetCurrentNumSkinnableBones();
			var bindPoses = new Matrix4x4[numSkinnableBones];
			var bones = new Transform[numSkinnableBones];
			var localToWorldMatrix = transform.localToWorldMatrix;
			for (int i = 0; i < numSkinnableBones && i < _ovrSkeleton.Bones.Count; ++i)
			{
				bones[i] = _ovrSkeleton.Bones[i].Transform;
				bindPoses[i] = _ovrSkeleton.BindPoses[i].Transform.worldToLocalMatrix * localToWorldMatrix;
			}
			_ovrMesh.Mesh.bindposes = bindPoses;
			_skinnedMeshRenderer.bones = bones;
			_skinnedMeshRenderer.updateWhenOffscreen = true;
		}

		_isInitialized = true;
	}

	private void Update()
	{
		if (_isInitialized)
		{
			bool shouldRender = false;

			if (_dataProvider != null)
			{
				var data = _dataProvider.GetMeshRendererData();

				shouldRender = data.IsDataValid && data.IsDataHighConfidence;
			}

			if (_skinnedMeshRenderer != null && _skinnedMeshRenderer.enabled != shouldRender)
			{
				_skinnedMeshRenderer.enabled = shouldRender;
			}
		}
	}
}
