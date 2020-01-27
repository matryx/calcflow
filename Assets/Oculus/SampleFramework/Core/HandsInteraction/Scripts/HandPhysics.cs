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
	public class HandPhysics : MonoBehaviour
	{
		// Stores all capsule colliders.
		private CapsuleInfo[] _capsules;
		private HandSkeleton _handSkeleton;
		private Hand _hand;
		private Transform _capsulesParent;
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

		private void Awake()
		{
			_handSkeleton = GetComponent<HandSkeleton>();
			_hand = GetComponent<Hand>();
			_capsulesParent = new GameObject("HandPhysics").transform;
			_capsulesParent.SetParent(_hand.transform.parent, false);
			_capsules = new CapsuleInfo[(int)OVRPlugin.SkeletonConstants.MaxBoneCapsules];
		}

		private void Start()
		{
			StartCoroutine(InitializeSkeleton());
		}

		private void OnDestroy()
		{
			Destroy(_capsulesParent);
		}

		private IEnumerator InitializeSkeleton()
		{
			bool success = false;
			while (!success)
			{
				var skeleton = new OVRPlugin.Skeleton();
				if (OVRPlugin.GetSkeleton(HandSkeleton.GetSkeletonTypeFromHandType(_hand.HandType), out skeleton))
				{
					success = InitializePhysics(ref skeleton);
				}
				yield return null;
			}
			IsInitialized = true;
		}

		/// <summary>
		/// Return all of the capsules that cover a bone (there can be multiple).
		/// </summary>
		public List<CapsuleInfo> GetCapsulesPerBone(OVRPlugin.BoneId boneId)
		{
			List<CapsuleInfo> capsulesPerBone = new List<CapsuleInfo>();
			foreach (var ovrCapsuleInfo in _capsules)
			{
				if (ovrCapsuleInfo.BoneIndex == boneId)
				{
					capsulesPerBone.Add(ovrCapsuleInfo);
				}
			}

			return capsulesPerBone;
		}

		public bool InitializePhysics(ref OVRPlugin.Skeleton skeleton)
		{
			if (!_handSkeleton.IsInitialized)
			{
				return false;
			}
			_capsulesParent.name = _hand.HandType + "Physics";
			// Each capsule is associated to a bone. there can be more than on capsule per bone.
			for (var ci = 0; ci < skeleton.NumBoneCapsules; ++ci)
			{
				var capsule = skeleton.BoneCapsules[ci];
				Transform bone = _handSkeleton.Bones[capsule.BoneIndex];
				// put the capsule in a separate flat hierarchy, because we can't have nested rigid bodies
				var capsuleObj = new GameObject(((
				  OVRPlugin.BoneId)capsule.BoneIndex).ToString() + "_Capsule");
				var t = capsuleObj.transform;

				// create the capsule collider
				var capsuleCollider = capsuleObj.AddComponent<CapsuleCollider>();
				var p0 = capsule.Points[0].FromFlippedZVector3f();
				var p1 = capsule.Points[1].FromFlippedZVector3f();
				var heading = p1 - p0;
				var distance = heading.magnitude;
				capsuleCollider.radius = capsule.Radius;
				capsuleCollider.height = distance + capsule.Radius * 2.0f;
				capsuleCollider.isTrigger = false;

				capsuleCollider.direction = 0;
				t.position = bone.position;
				t.rotation = bone.rotation;
				capsuleCollider.center = Vector3.Lerp(p0, p1, 0.5f);
				t.SetParent(_capsulesParent, true);

				// set up rigidBody
				var rigidBody = capsuleObj.AddComponent<Rigidbody>();
				rigidBody.mass = 1.0f;
				rigidBody.isKinematic = true;
				rigidBody.useGravity = false;
#if UNITY_2018_3_OR_NEWER
        rigidBody.collisionDetectionMode = CollisionDetectionMode.ContinuousSpeculative;
#else
				rigidBody.collisionDetectionMode = CollisionDetectionMode.Continuous;
#endif
				var capsuleInfo = capsuleObj.AddComponent<CapsuleInfo>();
				capsuleInfo.Init(_hand, (OVRPlugin.BoneId)capsule.BoneIndex, rigidBody, capsuleCollider);
				_capsules[ci] = capsuleInfo;
			}
			return true;
		}

		public void UpdatePose()
		{
			if (IsInitialized)
			{
				for (int ci = 0; ci < _capsules.Length; ++ci)
				{
					CapsuleInfo capsuleInfo = _capsules[ci];
					var go = capsuleInfo.CapsuleRigidBBody.gameObject;
					Transform bone = _handSkeleton.Bones[(int)capsuleInfo.BoneIndex];
					if (_hand.IsTracked)
					{
						if (go.activeSelf)
						{
							var rigidBody = capsuleInfo.CapsuleRigidBBody;
							rigidBody.MovePosition(bone.position);
							rigidBody.MoveRotation(bone.rotation);
						}
						else
						{
							go.SetActive(true);
							var rigidBody = capsuleInfo.CapsuleRigidBBody;
							rigidBody.position = bone.position;
							rigidBody.rotation = bone.rotation;
						}
					}
					else
					{
						go.SetActive(false);
					}
				}
			}
		}
	}
}
