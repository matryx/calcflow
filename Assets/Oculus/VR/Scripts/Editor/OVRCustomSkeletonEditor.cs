
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
using UnityEditor;
using UnityEditor.SceneManagement;
using BoneId = OVRSkeleton.BoneId;

[CustomEditor(typeof(OVRCustomSkeleton))]
public class OVRCustomSkeletonEditor : Editor
{
	public override void OnInspectorGUI()
	{
		DrawPropertiesExcluding(serializedObject, new string[] { "_customBones" });
		serializedObject.ApplyModifiedProperties();

		OVRCustomSkeleton skeleton = (OVRCustomSkeleton)target;
		OVRSkeleton.SkeletonType skeletonType = skeleton.GetSkeletonType();

		if (skeletonType == OVRSkeleton.SkeletonType.None)
		{
			EditorGUILayout.HelpBox("Please select a SkeletonType.", MessageType.Warning);
		}
		else
		{
			if (GUILayout.Button("Auto Map Bones"))
			{
				skeleton.TryAutoMapBonesByName();
				EditorUtility.SetDirty(skeleton);
				EditorSceneManager.MarkSceneDirty(skeleton.gameObject.scene);
			}

			EditorGUILayout.LabelField("Bones", EditorStyles.boldLabel);
			BoneId start = skeleton.GetCurrentStartBoneId();
			BoneId end = skeleton.GetCurrentEndBoneId();
			if (start != BoneId.Invalid && end != BoneId.Invalid)
			{
				for (int i = (int)start; i < (int)end; ++i)
				{
					string boneName = BoneLabelFromBoneId((BoneId)i);
					skeleton.CustomBones[i] = (Transform)EditorGUILayout.ObjectField(boneName, skeleton.CustomBones[i], typeof(Transform), true);
				}
			}
		}
	}

	// force aliased enum values to the more appropriate value
	private static string BoneLabelFromBoneId(BoneId boneId)
	{
		if (boneId == BoneId.Hand_Start)
		{
			return "Hand_WristRoot";
		}
		else if (boneId == BoneId.Hand_MaxSkinnable)
		{
			return "Hand_ThumbTip";
		}
		else
		{
			return boneId.ToString();
		}
	}
}

