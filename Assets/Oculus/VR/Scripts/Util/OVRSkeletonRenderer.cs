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

[DefaultExecutionOrder(-70)]
public class OVRSkeletonRenderer : MonoBehaviour
{
	public interface IOVRSkeletonRendererDataProvider
	{
		SkeletonRendererData GetSkeletonRendererData();
	}

	public struct SkeletonRendererData
	{
		public float RootScale { get; set; }
		public bool IsDataValid { get; set; }
		public bool IsDataHighConfidence { get; set; }
	}

	[SerializeField]
	private IOVRSkeletonRendererDataProvider _dataProvider;
	[SerializeField]
	private Material _skeletonMaterial;
	private const float LINE_RENDERER_WIDTH = 0.005f;
	private List<BoneVisualization> _boneVisualizations;
	private OVRSkeleton _ovrSkeleton;
	private GameObject _skeletonGO;
	private float _scale;
	private bool _isInitialized;

	private class BoneVisualization
	{
		public GameObject BoneGO;
		public Transform BoneBegin;
		public Transform BoneEnd;
		public LineRenderer Line;

		public BoneVisualization(GameObject rootGO, Material mat, float scale, Transform begin, Transform end)
		{
			BoneBegin = begin;
			BoneEnd = end;

			BoneGO = new GameObject(begin.name);
			BoneGO.transform.SetParent(rootGO.transform, false);

			Line = BoneGO.AddComponent<LineRenderer>();
			Line.sharedMaterial = mat;
			Line.useWorldSpace = true;
			Line.positionCount = 2;

			Line.SetPosition(0, BoneBegin.position);
			Line.SetPosition(1, BoneEnd.position);

			Line.startWidth = LINE_RENDERER_WIDTH * scale;
			Line.endWidth = LINE_RENDERER_WIDTH * scale;
		}

		public void Update(float scale, bool shouldRender)
		{
			Line.enabled = shouldRender;

			Line.SetPosition(0, BoneBegin.position);
			Line.SetPosition(1, BoneEnd.position);

			Line.startWidth = LINE_RENDERER_WIDTH * scale;
			Line.endWidth = LINE_RENDERER_WIDTH * scale;
		}
	}

	private void Awake()
	{
		if (_dataProvider == null)
		{
			_dataProvider = GetComponent<IOVRSkeletonRendererDataProvider>();
		}

		if (_ovrSkeleton == null)
		{
			_ovrSkeleton = GetComponent<OVRSkeleton>();
		}
	}

	private void Start()
	{
		if (_ovrSkeleton == null)
		{
			this.enabled = false;
			return;
		}

		Initialize();
	}

	private void Initialize()
	{
		_boneVisualizations = new List<BoneVisualization>();
		_ovrSkeleton = GetComponent<OVRSkeleton>();
		_skeletonGO = new GameObject("SkeletonRenderer");
		_skeletonGO.transform.SetParent(transform, false);

		if (_skeletonMaterial == null)
		{
			_skeletonMaterial = new Material(Shader.Find("Diffuse"));
		}

		for (int i = 0; i < _ovrSkeleton.Bones.Count; i++)
		{
			var boneVis = new BoneVisualization(
				_skeletonGO,
				_skeletonMaterial,
				_scale,
				_ovrSkeleton.Bones[i].Transform,
				_ovrSkeleton.Bones[i].Transform.parent);

			_boneVisualizations.Add(boneVis);
		}

		_isInitialized = true;
	}

	public void Update()
	{
		if (_isInitialized)
		{
			bool shouldRender = false;

			if (_dataProvider != null)
			{
				var data = _dataProvider.GetSkeletonRendererData();

				shouldRender = data.IsDataValid && data.IsDataHighConfidence;

				if (data.IsDataValid)
				{
					_scale = data.RootScale;
				}
			}

			for (int i = 0; i < _boneVisualizations.Count; i++)
			{
				_boneVisualizations[i].Update(_scale, shouldRender);
			}
		}
	}
}
