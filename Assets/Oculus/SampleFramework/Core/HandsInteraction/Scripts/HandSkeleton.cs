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

namespace OculusSampleFramework
{
	public class HandSkeleton : MonoBehaviour
	{

		//initalize _bones to MaxSkinnable
		private List<Transform> _bones;
		private Hand _hand;
		private GameObject _skeleton;
		private IList<Transform> _readOnlyBones;
		private bool _isInitialized = false;

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

		public IList<Transform> Bones
		{
			get
			{
				return _readOnlyBones;
			}
		}

		private void Awake()
		{
			_hand = GetComponent<Hand>();
			if (!_skeleton)
			{
				_skeleton = new GameObject("Skeleton");
				_skeleton.transform.SetParent(transform);
				_skeleton.transform.position = Vector3.zero;
				_skeleton.transform.rotation = Quaternion.identity;
			}
		}

		private void Start()
		{
			StartCoroutine(InitializeSkeleton());
		}

		private IEnumerator InitializeSkeleton()
		{
			bool success = false;
			while (!success)
			{
				var skeleton = new OVRPlugin.Skeleton();
				if (OVRPlugin.GetSkeleton(GetSkeletonTypeFromHandType(_hand.HandType), out skeleton))
				{
					success = InitializeSkeleton(ref skeleton);
				}
				yield return null;
			}
			if (GetComponent<HandMesh>() != null)
			{
				while (!AttacheBonesToMesh())
				{
					yield return null;
				}
			}
			IsInitialized = true;
		}

		public bool InitializeSkeleton(ref OVRPlugin.Skeleton skeleton)
		{
			_bones = new List<Transform>(new Transform[(int)OVRPlugin.BoneId.Max]);
			_readOnlyBones = _bones.AsReadOnly();

			for (int i = 0; i < skeleton.NumBones && i < skeleton.NumBones; ++i)
			{
				var boneGO = new GameObject((skeleton.Bones[i].Id).ToString());
				if (((OVRPlugin.BoneId)skeleton.Bones[i].ParentBoneIndex) == OVRPlugin.BoneId.Invalid)
				{
					boneGO.transform.SetParent(_skeleton.transform);
				}
				else if (_bones.Count > skeleton.Bones[i].ParentBoneIndex &&
				_bones[skeleton.Bones[i].ParentBoneIndex])
				{
					boneGO.transform.SetParent(_bones[skeleton.Bones[i].ParentBoneIndex].transform);
				}
				else
				{
					Debug.LogError("Cannot find bone parent");
				}
				boneGO.transform.localPosition = skeleton.Bones[i].Pose.Position.FromFlippedZVector3f();
				boneGO.transform.localRotation = skeleton.Bones[i].Pose.Orientation.FromFlippedZQuatf();

				_bones[i] = boneGO.transform;
			}
			return true;
		}

		private bool AttacheBonesToMesh()
		{
			if (!_hand.HandMesh || !_hand.HandMesh.IsInitialized)
			{
				return false;
			}
			var bindPoses = new Matrix4x4[(int)OVRPlugin.BoneId.Hand_MaxSkinnable];
			var skinnableBones = new Transform[(int)OVRPlugin.BoneId.Hand_MaxSkinnable];

			for (int i = 0; i < (int)OVRPlugin.BoneId.Hand_MaxSkinnable && i < _bones.Count; ++i)
			{
				skinnableBones[i] = _bones[i];
				bindPoses[i] = _bones[i].worldToLocalMatrix * transform.localToWorldMatrix;
			}

			_hand.Mesh.bindposes = bindPoses;
			_hand.HandSkinedMeshRenderer.bones = skinnableBones;
			return true;
		}

		public bool UpdatePose(OVRPlugin.HandState pose)
		{
			if (_bones == null)
			{
				return false;
			}

			if (_hand.IsTracked)
			{
				transform.position = pose.RootPose.Position.FromFlippedZVector3f();
				transform.rotation = pose.RootPose.Orientation.FromFlippedZQuatf();
				for (var i = 0; i < _bones.Count; ++i)
				{
					_bones[i].localRotation = pose.BoneRotations[i].FromFlippedZQuatf();
				}
				_hand.transform.localScale = new Vector3(pose.HandScale, pose.HandScale, pose.HandScale);
			}
			return true;
		}

		public static OVRPlugin.BoneId GetDistalBone(OVRPlugin.BoneId bi)
		{
			return (bi == OVRPlugin.BoneId.Hand_ThumbTip ? OVRPlugin.BoneId.Hand_Thumb3 :
					bi == OVRPlugin.BoneId.Hand_IndexTip ? OVRPlugin.BoneId.Hand_Index3 :
					bi == OVRPlugin.BoneId.Hand_MiddleTip ? OVRPlugin.BoneId.Hand_Middle3 :
					bi == OVRPlugin.BoneId.Hand_RingTip ? OVRPlugin.BoneId.Hand_Ring3 :
					bi == OVRPlugin.BoneId.Hand_PinkyTip ? OVRPlugin.BoneId.Hand_Pinky3 :
					  OVRPlugin.BoneId.Invalid);
		}

		public static bool IsDistalBone(OVRPlugin.BoneId bi)
		{
			return (bi == OVRPlugin.BoneId.Hand_Thumb3 ||
					bi == OVRPlugin.BoneId.Hand_Index3 ||
					bi == OVRPlugin.BoneId.Hand_Middle3 ||
					bi == OVRPlugin.BoneId.Hand_Ring3 ||
					bi == OVRPlugin.BoneId.Hand_Pinky3);
		}

		public static OVRPlugin.BoneId GetTip(OVRPlugin.BoneId bi)
		{
			OVRPlugin.BoneId tip = OVRPlugin.BoneId.Invalid;
			switch (bi)
			{
				case OVRPlugin.BoneId.Hand_Thumb0:
				case OVRPlugin.BoneId.Hand_Thumb1:
				case OVRPlugin.BoneId.Hand_Thumb2:
				case OVRPlugin.BoneId.Hand_Thumb3:
				case OVRPlugin.BoneId.Hand_ThumbTip:
					{
						tip = OVRPlugin.BoneId.Hand_ThumbTip;
						break;
					}
				case OVRPlugin.BoneId.Hand_Index1:
				case OVRPlugin.BoneId.Hand_Index2:
				case OVRPlugin.BoneId.Hand_Index3:
				case OVRPlugin.BoneId.Hand_IndexTip:
					{
						tip = OVRPlugin.BoneId.Hand_IndexTip;
						break;
					}
				case OVRPlugin.BoneId.Hand_Middle1:
				case OVRPlugin.BoneId.Hand_Middle2:
				case OVRPlugin.BoneId.Hand_Middle3:
				case OVRPlugin.BoneId.Hand_MiddleTip:
					{
						tip = OVRPlugin.BoneId.Hand_MiddleTip;
						break;
					}
				case OVRPlugin.BoneId.Hand_Ring1:
				case OVRPlugin.BoneId.Hand_Ring2:
				case OVRPlugin.BoneId.Hand_Ring3:
				case OVRPlugin.BoneId.Hand_RingTip:
					{
						tip = OVRPlugin.BoneId.Hand_RingTip;
						break;
					}
				case OVRPlugin.BoneId.Hand_Pinky0:
				case OVRPlugin.BoneId.Hand_Pinky1:
				case OVRPlugin.BoneId.Hand_Pinky2:
				case OVRPlugin.BoneId.Hand_Pinky3:
				case OVRPlugin.BoneId.Hand_PinkyTip:
					{
						tip = OVRPlugin.BoneId.Hand_PinkyTip;
						break;
					}
				default:
					{
						tip = OVRPlugin.BoneId.Invalid;
						break;
					}
			}
			return tip;
		}

		public static int GetTipParentIndex(OVRPlugin.BoneId tip)
		{
			if (tip == OVRPlugin.BoneId.Hand_ThumbTip || tip == OVRPlugin.BoneId.Hand_MaxSkinnable)
			{
				return (int)OVRPlugin.BoneId.Hand_Thumb3;
			}
			else if (tip == OVRPlugin.BoneId.Hand_IndexTip)
			{
				return (int)OVRPlugin.BoneId.Hand_Index3;
			}
			else if (tip == OVRPlugin.BoneId.Hand_MiddleTip)
			{
				return (int)OVRPlugin.BoneId.Hand_Middle3;
			}
			else if (tip == OVRPlugin.BoneId.Hand_RingTip)
			{
				return (int)OVRPlugin.BoneId.Hand_Ring3;
			}
			else if (tip == OVRPlugin.BoneId.Hand_PinkyTip)
			{
				return (int)OVRPlugin.BoneId.Hand_Pinky3;
			}
			else
			{
				return (int)OVRPlugin.BoneId.Invalid;
			}
		}

		public static bool IsDistal(OVRPlugin.BoneId bone)
		{
			if (bone == OVRPlugin.BoneId.Hand_Thumb3)
			{
				return true;
			}
			else if (bone == OVRPlugin.BoneId.Hand_Index3)
			{
				return true;
			}
			else if (bone == OVRPlugin.BoneId.Hand_Middle3)
			{
				return true;
			}
			else if (bone == OVRPlugin.BoneId.Hand_Ring3)
			{
				return true;
			}
			else if (bone == OVRPlugin.BoneId.Hand_Pinky3)
			{
				return true;
			}
			else
			{
				return false;
			}
		}

		public static OVRPlugin.SkeletonType GetSkeletonTypeFromMeshType(OVRPlugin.MeshType meshType)
		{
			return meshType == OVRPlugin.MeshType.HandLeft ?
			  OVRPlugin.SkeletonType.HandLeft :
			  meshType == OVRPlugin.MeshType.HandRight ?
			  OVRPlugin.SkeletonType.HandRight : OVRPlugin.SkeletonType.None;
		}

		public static OVRPlugin.SkeletonType GetSkeletonTypeFromHandType(OVRPlugin.Hand hand)
		{
			return hand == OVRPlugin.Hand.HandLeft ?
			  OVRPlugin.SkeletonType.HandLeft :
			  hand == OVRPlugin.Hand.HandRight ?
			  OVRPlugin.SkeletonType.HandRight : OVRPlugin.SkeletonType.None;
		}

		public static OVRPlugin.Hand GetHandFromMeshType(OVRPlugin.MeshType meshType)
		{
			return meshType == OVRPlugin.MeshType.HandLeft ?
			  OVRPlugin.Hand.HandLeft :
			  meshType == OVRPlugin.MeshType.HandRight ?
			  OVRPlugin.Hand.HandRight : OVRPlugin.Hand.None;
		}
	}
}
