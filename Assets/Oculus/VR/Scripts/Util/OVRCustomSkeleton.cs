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
public class OVRCustomSkeleton : OVRSkeleton
{
	[SerializeField]
	private List<Transform> _customBones = new List<Transform>(new Transform[(int)BoneId.Max]);

#if UNITY_EDITOR
	private static readonly string[] _fbxBoneNames =
	{
		"wrist",
		"forearm_stub",
		"thumb0",
		"thumb1",
		"thumb2",
		"thumb3",
		"index1",
		"index2",
		"index3",
		"middle1",
		"middle2",
		"middle3",
		"ring1",
		"ring2",
		"ring3",
		"pinky0",
		"pinky1",
		"pinky2",
		"pinky3"
	};

	private static readonly string[] _fbxFingerNames =
	{
		"thumb",
		"index",
		"middle",
		"ring",
		"pinky"
	};
	private static readonly string[] _handPrefix = { "l_", "r_" };
#endif

	public List<Transform> CustomBones { get { return _customBones; } }

#if UNITY_EDITOR
	public void TryAutoMapBonesByName()
	{
		BoneId start = GetCurrentStartBoneId();
		BoneId end = GetCurrentEndBoneId();
		SkeletonType skeletonType = GetSkeletonType();
		if (start != BoneId.Invalid && end != BoneId.Invalid)
		{
			for (int bi = (int)start; bi < (int)end; ++bi)
			{
				string fbxBoneName = FbxBoneNameFromBoneId(skeletonType, (BoneId)bi);
				Transform t = transform.FindChildRecursive(fbxBoneName);

				if (t != null)
				{
					_customBones[(int)bi] = t;
				}
			}
		}
	}

	private static string FbxBoneNameFromBoneId(SkeletonType skeletonType, BoneId bi)
	{
		if (bi >= BoneId.Hand_ThumbTip && bi <= BoneId.Hand_PinkyTip)
		{
			return _handPrefix[(int)skeletonType] + _fbxFingerNames[(int)bi - (int)BoneId.Hand_ThumbTip] + "_finger_tip_marker";
		}
		else
		{
			return "b_" + _handPrefix[(int)skeletonType] + _fbxBoneNames[(int)bi];
		}
	}

	
#endif
	
	protected override void InitializeBones(OVRPlugin.Skeleton skeleton)
	{
		_bones = new List<OVRBone>(new OVRBone[skeleton.NumBones]);
		Bones = _bones.AsReadOnly();

		for (int i = 0; i < skeleton.NumBones; ++i)
		{
			BoneId id = (BoneId)skeleton.Bones[i].Id;
			short parentIdx = skeleton.Bones[i].ParentBoneIndex;
			Transform t = _customBones[(int)id];
			_bones[i] = new OVRBone(id, parentIdx, t);
		}
	}
}
